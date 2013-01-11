{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

-- | The Haskell→Javascript compiler.

module Language.Fay.Compiler
  (runCompile
  ,compileViaStr
  ,compileForDocs
  ,compileToAst
  ,compileModule
  ,compileExp
  ,compileDecl
  ,printCompile
  ,printTestCompile
  ,compileToplevelModule)
  where

import           Language.Fay.Compiler.FFI
import           Language.Fay.Compiler.Misc
import           Language.Fay.Compiler.Optimizer
import           Language.Fay.ModuleScope        (bindAsLocals, findTopLevelNames, moduleLocals)
import           Language.Fay.Print              (printJSString)
import           Language.Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.IO
import           Control.Monad.State
import           Control.Monad.RWS
import           Data.Default                    (def)
import           Data.List
import           Data.List.Extra
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import           Data.Maybe
import qualified GHC.Paths                       as GHCPaths
import           Language.Haskell.Exts
import           System.Directory                (doesFileExist)
import           System.FilePath                 ((</>))
import           System.Process.Extra

--------------------------------------------------------------------------------
-- Top level entry points

-- | Run the compiler.
runCompile :: CompileReader -> CompileState
           -> Compile a
           -> IO (Either CompileError (a,CompileState))
runCompile reader state m =
  fmap (fmap dropWriter)
       (runErrorT (runRWST (unCompile m) reader state))

  where dropWriter (a,s,_w) = (a,s)

-- | Compile a Haskell source string to a JavaScript source string.
compileViaStr :: (Show from,Show to,CompilesTo from to)
              => FilePath
              -> CompileConfig
              -> (from -> Compile to)
              -> String
              -> IO (Either CompileError (PrintState,CompileState))
compileViaStr filepath config with from = do
  cs <- defaultCompileState
  rs <- defaultCompileReader config
  runCompile rs
             (cs { stateFilePath = filepath })
             (parseResult (throwError . uncurry ParseError)
                          (fmap (\x -> execState (runPrinter (printJS x)) printConfig) . with)
                          (parseFay filepath from))

  where printConfig = def { psPretty = configPrettyPrint config }

-- | Compile a Haskell source string to a JavaScript source string.
compileToAst :: (Show from,Show to,CompilesTo from to)
              => FilePath
              -> CompileReader
              -> CompileState
              -> (from -> Compile to)
              -> String
              -> IO (Either CompileError (to,CompileState))
compileToAst filepath reader state with from =
  runCompile reader
             state
             (parseResult (throwError . uncurry ParseError)
                          with
                          (parseFay filepath from))

-- | Parse some Fay code.
parseFay :: Parseable ast => FilePath -> String -> ParseResult ast
parseFay filepath = parseWithMode parseMode { parseFilename = filepath } . applyCPP

-- | Apply incredibly simplistic CPP handling. It only recognizes the following:
--
-- > #if FAY
-- > #ifdef FAY
-- > #ifndef FAY
-- > #else
-- > #endif
--
-- Note that this implementation replaces all removed lines with blanks, so
-- that line numbers remain accurate.
applyCPP :: String -> String
applyCPP =
    unlines . loop NoCPP . lines
  where
    loop _ [] = []
    loop state ("#if FAY":rest) = "" : loop (CPPIf True state) rest
    loop state ("#ifdef FAY":rest) = "" : loop (CPPIf True state) rest
    loop state ("#ifndef FAY":rest) = "" : loop (CPPIf False state) rest
    loop (CPPIf b oldState) ("#else":rest) = "" : loop (CPPElse (not b) oldState) rest
    loop (CPPIf _ oldState) ("#endif":rest) = "" : loop oldState rest
    loop (CPPElse _ oldState) ("#endif":rest) = "" : loop oldState rest
    loop state (x:rest) = (if toInclude state then x else "") : loop state rest

    toInclude NoCPP = True
    toInclude (CPPIf x state) = x && toInclude state
    toInclude (CPPElse x state) = x && toInclude state

data CPPState = NoCPP
              | CPPIf Bool CPPState
              | CPPElse Bool CPPState

-- | The parse mode for Fay.
parseMode :: ParseMode
parseMode = defaultParseMode { extensions =
  [GADTs,StandaloneDeriving,PackageImports,EmptyDataDecls,TypeOperators,RecordWildCards,NamedFieldPuns] }

-- | Compile the given input and print the output out prettily.
printCompile :: (Show from,Show to,CompilesTo from to)
              => CompileConfig
              -> (from -> Compile to)
              -> String
              -> IO ()
printCompile config with from = do
  result <- compileViaStr "<interactive>" config { configPrettyPrint = True } with from
  case result of
    Left err -> print err
    Right (PrintState{..},_) -> do
      putStrLn (concat (reverse (psOutput)))

-- | Compile a String of Fay and print it as beautified JavaScript.
printTestCompile :: String -> IO ()
printTestCompile = printCompile def { configWarn = False } (compileModule False)

-- | Compile the given Fay code for the documentation. This is
-- specialised because the documentation isn't really “real”
-- compilation.
compileForDocs :: Module -> Compile [JsStmt]
compileForDocs mod = do
  initialPass mod
  compileModule False mod

-- | Compile the top-level Fay module.
compileToplevelModule :: Module -> Compile [JsStmt]
compileToplevelModule mod@(Module _ (ModuleName modulename) _ _ _ _ _)  = do
  cfg <- config id
  -- Remove the fay source dir from the includes, -package fay is already supplied.
  -- This will prevent errors on FFI instance declarations.
  faydir <- io faySourceDir
  let includeDirs = filter (/= faydir) (configDirectoryIncludes cfg)

  when (configTypecheck cfg) $
    typecheck (configPackageConf cfg) includeDirs [] (configWall cfg) $
      fromMaybe modulename $ configFilePath cfg
  initialPass mod
  cs <- io defaultCompileState
  modify $ \s -> s { stateImported = stateImported cs }
  stmts <- compileModule True mod
  fay2js <- do syms <- gets stateFayToJs
               return $ if null syms then [] else [fayToJsDispatcher syms]
  js2fay <- do syms <- gets stateJsToFay
               return $ if null syms then [] else [jsToFayDispatcher syms]
  let maybeOptimize = if configOptimize cfg then runOptimizer optimizeToplevel else id
  conses <- gets stateCons
  if configDispatcherOnly cfg
     then return (maybeOptimize (conses ++ fay2js ++ js2fay))
     else return (maybeOptimize (stmts ++
                    if configDispatchers cfg then conses ++ fay2js ++ js2fay else []))

--------------------------------------------------------------------------------
-- Initial pass-through collecting record definitions

initialPass :: Module -> Compile ()
initialPass (Module _ _ _ Nothing _ imports decls) = do
  mapM_ initialPass_import imports
  mapM_ initialPass_decl decls
initialPass mod = throwError (UnsupportedModuleSyntax mod)

initialPass_import :: ImportDecl -> Compile ()
initialPass_import (ImportDecl _ _ _ _ Just{} _ _) = do
  return ()
--  warn $ "import with package syntax ignored: " ++ prettyPrint i
initialPass_import (ImportDecl _ name False _ Nothing Nothing _) = do
  void $ initialPass_unlessImported name $ \filepath contents -> do
    state <- get
    reader <- ask
    result <- liftIO $ initialPass_records filepath reader state initialPass contents
    case result of
      Right ((),st) -> do
        -- Merges the state gotten from passing through an imported
        -- module with the current state. We can assume no duplicate
        -- records exist since GHC would pick that up.
        modify $ \s -> s { stateRecords = stateRecords st
                         , stateRecordTypes = stateRecordTypes st
                         , stateImported = stateImported st
                         }
      Left err -> throwError err
    return ()
initialPass_import i = throwError $ UnsupportedImport i

-- | Don't re-import the same modules.
initialPass_unlessImported :: ModuleName
                           -> (FilePath -> String -> Compile ())
                           -> Compile ()
initialPass_unlessImported name importIt = do
  imported <- gets stateImported
  case lookup name imported of
    Just _ -> return ()
    Nothing -> do
      dirs <- configDirectoryIncludes <$> config id
      (filepath,contents) <- findImport dirs name
      modify $ \s -> s { stateImported = (name,filepath) : imported }
      importIt filepath contents

initialPass_records :: (Show from,Parseable from)
                    => FilePath
                    -> CompileReader
                    -> CompileState
                    -> (from -> Compile ())
                    -> String
                    -> IO (Either CompileError ((),CompileState))
initialPass_records filepath compileReader compileState with from =
  runCompile compileReader
             compileState
             (parseResult (throwError . uncurry ParseError)
                          with
                          (parseFay filepath from))

initialPass_decl :: Decl -> Compile ()
initialPass_decl decl = do
  case decl of
    DataDecl _loc DataType _ctx name _tyvarb qualcondecls _deriv -> do
      let ns = flip map qualcondecls (\(QualConDecl _loc' _tyvarbinds _ctx' condecl) -> conDeclName condecl)
      addRecordTypeState name ns
    _ -> return ()


  case decl of
    DataDecl _ DataType _ _ _ constructors _ -> initialPass_dataDecl constructors
    GDataDecl _ DataType _l _i _v _n decls _ -> initialPass_dataDecl (map convertGADT decls)
    _ -> return ()

  where
    addRecordTypeState name cons = modify $ \s -> s
      { stateRecordTypes = (UnQual name, map UnQual cons) : stateRecordTypes s }

    conDeclName (ConDecl n _) = n
    conDeclName (InfixConDecl _ n _) = n
    conDeclName (RecDecl n _) = n


-- | Collect record definitions and store record name and field names.
-- A ConDecl will have fields named slot1..slotN
initialPass_dataDecl :: [QualConDecl] -> Compile ()
initialPass_dataDecl constructors = do
  forM_ constructors $ \(QualConDecl _ _ _ condecl) ->
    case condecl of
      ConDecl name types -> do
        let fields =  map (Ident . ("slot"++) . show . fst) . zip [1 :: Integer ..] $ types
        addRecordState name fields
      InfixConDecl _t1 name _t2 ->
        addRecordState name ["slot1", "slot2"]
      RecDecl name fields' -> do
        let fields = concatMap fst fields'
        addRecordState name fields

  where
    addRecordState :: Name -> [Name] -> Compile ()
    addRecordState name fields = modify $ \s -> s
      { stateRecords = (UnQual name,map UnQual fields) : stateRecords s }

--------------------------------------------------------------------------------
-- Typechecking

typecheck :: Maybe FilePath -> [FilePath] -> [String] -> Bool -> String -> Compile ()
typecheck packageConf includeDirs ghcFlags wall fp = do
  ghcPackageDbArgs <-
    case packageConf of
      Nothing -> return []
      Just pk -> do
        flag <- liftIO getGhcPackageDbFlag
        return [flag ++ '=' : pk]
  let flags =
          [ "-fno-code"
          , "-hide-package base"
          , "-package fay-base"
          , "-cpp", "-DFAY=1"
          , "-main-is"
          , "Language.Fay.DummyMain"
          , "-i" ++ concat (intersperse "," includeDirs)
          , fp ] ++ ghcPackageDbArgs ++ ghcFlags ++ wallF
  res <- liftIO $ readAllFromProcess GHCPaths.ghc flags ""
  either error (warn . fst) res
   where
    wallF | wall = ["-Wall"]
          | otherwise = []

--------------------------------------------------------------------------------
-- Compilers

-- | Compile Haskell module.
compileModule :: Bool -> Module -> Compile [JsStmt]
compileModule toplevel (Module _ modulename _pragmas Nothing exports imports decls) =
  withModuleScope $ do
    modify $ \s -> s { stateModuleName = modulename
                     , stateExports = []
                     , stateModuleScope = findTopLevelNames modulename decls
                     }
    imported <- fmap concat (mapM compileImport imports)
    current <- compileDecls True decls

    case exports of
      Just exps -> mapM_ emitExport exps
      Nothing -> do
        exps <- moduleLocals modulename <$> gets stateModuleScope
        modify $ \s -> s { stateExports = exps ++ stateExports s }

    exportStdlib     <- config configExportStdlib
    exportStdlibOnly <- config configExportStdlibOnly
    if exportStdlibOnly
       then if anStdlibModule modulename || toplevel
               then if toplevel
                       then return imported
                       else return (current ++ imported)
               else return []
       else if not exportStdlib && anStdlibModule modulename
               then return []
               else return (imported ++ current)
compileModule _ mod = throwError (UnsupportedModuleSyntax mod)

instance CompilesTo Module [JsStmt] where compileTo = compileModule False

-- | Is the module a standard module, i.e., one that we'd rather not
-- output code for if we're compiling separate files.
anStdlibModule :: ModuleName -> Bool
anStdlibModule (ModuleName name) = elem name ["Prelude","FFI","Language.Fay.FFI","Data.Data"]

findImport :: [FilePath] -> ModuleName -> Compile (FilePath,String)
findImport alldirs mname = go alldirs mname where
  go (dir:dirs) name = do
    exists <- io (doesFileExist path)
    if exists
      then fmap (path,) (fmap stdlibHack (io (readFile path)))
      else go dirs name
    where
      path = dir </> replace '.' '/' (prettyPrint name) ++ ".hs"
      replace c r = map (\x -> if x == c then r else x)
  go [] name =
    throwError $ Couldn'tFindImport name alldirs

  stdlibHack
    | mname == ModuleName "Language.Fay.Stdlib" = \s -> s ++ "\n\ndata Maybe a = Just a | Nothing"
    | mname == ModuleName "Language.Fay.FFI"    = const "module Language.Fay.FFI where\n\ndata Nullable a = Nullable a | Null\n\ndata Defined a = Defined a | Undefined"
    | otherwise = id

-- | Compile the given import.
compileImport :: ImportDecl -> Compile [JsStmt]
compileImport (ImportDecl _ _ _ _ Just{} _ _) = do
--  warn $ "import with package syntax ignored: " ++ prettyPrint i
  return []
compileImport (ImportDecl _ name False _ Nothing Nothing Nothing) =
  compileImportWithFilter name (const $ return True)
compileImport (ImportDecl _ name False _ Nothing Nothing (Just (True, specs))) =
  compileImportWithFilter name (fmap not . imported specs)
compileImport (ImportDecl _ name False _ Nothing Nothing (Just (False, specs))) =
  compileImportWithFilter name (imported specs)
compileImport i =
  throwError $ UnsupportedImport i

imported :: [ImportSpec] -> QName -> Compile Bool
imported is qn = anyM (matching qn) is
  where
    matching :: QName -> ImportSpec -> Compile Bool
    matching (Qual _ _) (IAbs _) = return True -- Types are always OK
    matching (Qual _ name) (IVar var) = return $ name == var
    matching (Qual _ name) (IThingAll typ) = do
      recs <- typeToRecs $ UnQual typ
      if UnQual name `elem` recs
        then return True
        else do
          fields <- typeToFields $ UnQual typ
          return $ UnQual name `elem` fields
    matching (Qual _ name) (IThingWith typ cns) =
      flip anyM cns $ \cn -> case cn of
        ConName _ -> do
          recs <- typeToRecs $ UnQual typ
          return $ UnQual name `elem` recs
        VarName _ -> do
          fields <- typeToFields $ UnQual typ
          return $ UnQual name `elem` fields
    matching q is = error $ "compileImport: Unsupported QName ImportSpec combination " ++ show (q, is) ++ ", this is a bug!"


compileImportWithFilter :: ModuleName -> (QName -> Compile Bool) -> Compile [JsStmt]
compileImportWithFilter name importFilter =
  unlessImported name importFilter $ \filepath contents -> do
    state <- get
    reader <- ask
    result <- liftIO $ compileToAst filepath reader state (compileModule False) contents
    case result of
      Right (stmts,state) -> do
        imports <- filterM importFilter $ stateExports state
        modify $ \s -> s { stateFayToJs     = stateFayToJs state
                         , stateJsToFay     = stateJsToFay state
                         , stateImported    = stateImported state
                         , stateLocalScope  = S.empty
                         , stateModuleScope = bindAsLocals imports (stateModuleScope s)
                         , stateCons        = stateCons state
                         }
        return stmts
      Left err -> throwError err

unlessImported :: ModuleName
               -> (QName -> Compile Bool)
               -> (FilePath -> String -> Compile [JsStmt])
               -> Compile [JsStmt]
unlessImported "Language.Fay.Types" _ _ = return []
unlessImported name importFilter importIt = do
  imported <- gets stateImported
  case lookup name imported of
    Just _ -> do
      sec <- gets stateExportsCache
      case M.lookup name sec of
        Nothing -> throwError $ UnableResolveCachedImport name
        Just exports -> do
          imports <- filterM importFilter exports
          modify $ \s -> s { stateModuleScope = bindAsLocals imports (stateModuleScope s) }
          return []
    Nothing -> do
      dirs <- configDirectoryIncludes <$> config id
      (filepath,contents) <- findImport dirs name
      res <- importIt filepath contents
      exportsCache <- gets stateExports
                         -- TODO stateImported is already added in initialPass so it is not needed here
                         -- but one Api test fails if it's removed.
      modify $ \s -> s { stateImported     = (name,filepath) : imported
                       , stateExportsCache = M.insert name exportsCache (stateExportsCache s)
                       }
      return res

-- | Compile Haskell declaration.
compileDecls :: Bool -> [Decl] -> Compile [JsStmt]
compileDecls toplevel decls =
  case decls of
    [] -> return []
    (TypeSig _ _ sig:bind@PatBind{}:decls) -> appendM (scoped (compilePatBind toplevel (Just sig) bind))
                                                      (compileDecls toplevel decls)
    (decl:decls) -> appendM (scoped (compileDecl toplevel decl))
                            (compileDecls toplevel decls)

  where appendM m n = do x <- m
                         xs <- n
                         return (x ++ xs)
        scoped = if toplevel then withScope else id

-- | Compile a declaration.
compileDecl :: Bool -> Decl -> Compile [JsStmt]
compileDecl toplevel decl =
  case decl of
    pat@PatBind{} -> compilePatBind toplevel Nothing pat
    FunBind matches -> compileFunCase toplevel matches
    DataDecl _ DataType _ _ _ constructors _ -> compileDataDecl toplevel decl constructors
    GDataDecl _ DataType _l _i _v _n decls _ -> compileDataDecl toplevel decl (map convertGADT decls)
    -- Just ignore type aliases and signatures.
    TypeDecl{} -> return []
    TypeSig{} -> return []
    InfixDecl{} -> return []
    ClassDecl{} -> return []
    InstDecl{} -> return [] -- FIXME: Ignore.
    DerivDecl{} -> return []
    _ -> throwError (UnsupportedDeclaration decl)

instance CompilesTo Decl [JsStmt] where compileTo = compileDecl True

-- | Compile a top-level pattern bind.
compilePatBind :: Bool -> Maybe Type -> Decl -> Compile [JsStmt]
compilePatBind toplevel sig pat =
  case pat of
    PatBind srcloc (PVar ident) Nothing (UnGuardedRhs rhs) (BDecls []) ->
      case ffiExp rhs of
        Just formatstr -> case sig of
          Just sig -> compileFFI srcloc ident formatstr sig
          Nothing  -> throwError (FfiNeedsTypeSig pat)
        _ -> compileUnguardedRhs srcloc toplevel ident rhs
    PatBind srcloc (PVar ident) Nothing (UnGuardedRhs rhs) bdecls -> do
      compileUnguardedRhs srcloc toplevel ident (Let bdecls rhs)
    _ -> throwError (UnsupportedDeclaration pat)

  where ffiExp (App (Var (UnQual (Ident "ffi"))) (Lit (String formatstr))) = Just formatstr
        ffiExp _ = Nothing

-- | Compile a normal simple pattern binding.
compileUnguardedRhs :: SrcLoc -> Bool -> Name -> Exp -> Compile [JsStmt]
compileUnguardedRhs srcloc toplevel ident rhs = do
  unless toplevel $ bindVar ident
  withScope $ do
    body <- compileExp rhs
    bind <- bindToplevel srcloc toplevel ident (thunk body)
    return [bind]

convertGADT :: GadtDecl -> QualConDecl
convertGADT d =
  case d of
    GadtDecl srcloc name typ -> QualConDecl srcloc tyvars context
                                            (ConDecl name (convertFunc typ))
  where tyvars = []
        context = []
        convertFunc (TyCon _) = []
        convertFunc (TyFun x xs) = UnBangedTy x : convertFunc xs
        convertFunc (TyParen x) = convertFunc x
        convertFunc _ = []

-- | Compile a data declaration.
compileDataDecl :: Bool -> Decl -> [QualConDecl] -> Compile [JsStmt]
compileDataDecl toplevel _decl constructors =
  fmap concat $
    forM constructors $ \(QualConDecl srcloc _ _ condecl) ->
      case condecl of
        ConDecl name types  -> do
          let fields =  map (Ident . ("slot"++) . show . fst) . zip [1 :: Integer ..] $ types
              fields' = (zip (map return fields) types)
          cons <- makeConstructor name fields
          func <- makeFunc name fields
          emitFayToJs name fields'
          emitJsToFay name fields'
          emitCons cons
          return [func]
        InfixConDecl t1 name t2 -> do
          let slots = ["slot1","slot2"]
              fields = zip (map return slots) [t1, t2]
          cons <- makeConstructor name slots
          func <- makeFunc name slots
          emitFayToJs name fields
          emitJsToFay name fields
          emitCons cons
          return [func]
        RecDecl name fields' -> do
          let fields = concatMap fst fields'
          cons <- makeConstructor name fields
          func <- makeFunc name fields
          funs <- makeAccessors srcloc fields
          emitFayToJs name fields'
          emitJsToFay name fields'
          emitCons cons
          return (func : funs)

  where
    emitCons cons = modify $ \s -> s { stateCons = cons : stateCons s }

    -- Creates a constructor R_RecConstr for a Record
    makeConstructor :: Name -> [Name] -> Compile JsStmt
    makeConstructor name (map (JsNameVar . UnQual) -> fields) = do
      qname <- qualify name
      emitExport (EVar qname)
      return $
        JsVar (JsConstructor qname) $
          JsFun fields (for fields $ \field -> JsSetProp JsThis field (JsName field))
            Nothing

    -- Creates a function to initialize the record by regular application
    makeFunc :: Name -> [Name] -> Compile JsStmt
    makeFunc name (map (JsNameVar . UnQual) -> fields) = do
      let fieldExps = map JsName fields
      qname <- qualify name
      return $ JsVar (JsNameVar qname) $
        foldr (\slot inner -> JsFun [slot] [] (Just inner))
          (thunk $ JsNew (JsConstructor qname) fieldExps)
          fields

    -- Creates getters for a RecDecl's values
    makeAccessors :: SrcLoc -> [Name] -> Compile [JsStmt]
    makeAccessors srcloc fields =
      forM fields $ \name ->
           bindToplevel srcloc
                        toplevel
                        name
                        (JsFun [JsNameVar "x"]
                               []
                               (Just (thunk (JsGetProp (force (JsName (JsNameVar "x")))
                                                       (JsNameVar (UnQual name))))))

-- | Compile a function which pattern matches (causing a case analysis).
compileFunCase :: Bool -> [Match] -> Compile [JsStmt]
compileFunCase _toplevel [] = return []
compileFunCase toplevel matches@(Match srcloc name argslen _ _ _:_) = do
  pats <- fmap optimizePatConditions (mapM compileCase matches)
  bindVar name
  bind <- bindToplevel srcloc
                       toplevel
                       name
                       (foldr (\arg inner -> JsFun [arg] [] (Just inner))
                              (stmtsThunk (concat pats ++ basecase))
                              args)
  return [bind]
  where args = zipWith const uniqueNames argslen

        isWildCardMatch (Match _ _ pats _ _ _) = all isWildCardPat pats

        compileCase :: Match -> Compile [JsStmt]
        compileCase match@(Match _ _ pats _ rhs _) = do
          withScope $ do
            whereDecls' <- whereDecls match
            generateScope $ zipWithM (\arg pat -> compilePat (JsName arg) pat []) args pats
            generateScope $ mapM compileLetDecl whereDecls'
            rhsform <- compileRhs rhs
            body <- if null whereDecls'
                      then return $ either id JsEarlyReturn rhsform
                      else do
                          binds <- mapM compileLetDecl whereDecls'
                          return $ case rhsform of
                            Right exp ->
                              (JsEarlyReturn (JsApp (JsFun [] (concat binds) (Just exp)) []))
                            Left stmt ->
                              (JsEarlyReturn (JsApp (JsFun [] (concat binds ++ [stmt]) Nothing) []))
            foldM (\inner (arg,pat) ->
                    compilePat (JsName arg) pat inner)
                  [body]
                  (zip args pats)

        whereDecls :: Match -> Compile [Decl]
        whereDecls (Match _ _ _ _ _ (BDecls decls)) = return decls
        whereDecls match = throwError (UnsupportedWhereInMatch match)

        basecase :: [JsStmt]
        basecase = if any isWildCardMatch matches
                      then []
                      else [throw ("unhandled case in " ++ prettyPrint name)
                                  (JsList (map JsName args))]

-- | Compile a right-hand-side expression.
compileRhs :: Rhs -> Compile (Either JsStmt JsExp)
compileRhs (UnGuardedRhs exp) = Right <$> compileExp exp
compileRhs (GuardedRhss rhss) = Left <$> compileGuards rhss

-- | Compile guards
compileGuards :: [GuardedRhs] -> Compile JsStmt
compileGuards ((GuardedRhs _ (Qualifier (Var (UnQual (Ident "otherwise"))):_) exp):_) =
  (\e -> JsIf (JsLit (JsBool True)) [JsEarlyReturn e] []) <$> compileExp exp
compileGuards (GuardedRhs _ (Qualifier guard:_) exp : rest) =
  makeIf <$> fmap force (compileExp guard)
         <*> compileExp exp
         <*> if null rest then (return []) else do
           gs' <- compileGuards rest
           return [gs']
    where makeIf gs e gss = JsIf gs [JsEarlyReturn e] gss

compileGuards rhss = throwError . UnsupportedRhs . GuardedRhss $ rhss

-- | Compile Haskell expression.
compileExp :: Exp -> Compile JsExp
compileExp exp =
  case exp of
    Paren exp                     -> compileExp exp
    Var qname                     -> compileVar qname
    Lit lit                       -> compileLit lit
    App exp1 exp2                 -> compileApp exp1 exp2
    NegApp exp                    -> compileNegApp exp
    InfixApp exp1 op exp2         -> compileInfixApp exp1 op exp2
    Let (BDecls decls) exp        -> compileLet decls exp
    List []                       -> return JsNull
    List xs                       -> compileList xs
    Tuple xs                      -> compileList xs
    If cond conseq alt            -> compileIf cond conseq alt
    Case exp alts                 -> compileCase exp alts
    Con (UnQual (Ident "True"))   -> return (JsLit (JsBool True))
    Con (UnQual (Ident "False"))  -> return (JsLit (JsBool False))
    Con qname                     -> compileVar qname
    Do stmts                      -> compileDoBlock stmts
    Lambda _ pats exp             -> compileLambda pats exp
    LeftSection e o               -> compileExp =<< desugarLeftSection e o
    RightSection o e              -> compileExp =<< desugarRightSection o e
    EnumFrom i                    -> compileEnumFrom i
    EnumFromTo i i'               -> compileEnumFromTo i i'
    EnumFromThen a b              -> compileEnumFromThen a b
    EnumFromThenTo a b z          -> compileEnumFromThenTo a b z
    RecConstr name fieldUpdates   -> compileRecConstr name fieldUpdates
    RecUpdate rec  fieldUpdates   -> updateRec rec fieldUpdates
    ListComp exp stmts            -> compileExp =<< desugarListComp exp stmts
    ExpTypeSig _ e _ -> compileExp e

    exp -> throwError (UnsupportedExpression exp)

instance CompilesTo Exp JsExp where compileTo = compileExp

compileVar :: QName -> Compile JsExp
compileVar qname = do
  qname <- resolveName qname
  return (JsName (JsNameVar qname))

-- | Compile simple application.
compileApp :: Exp -> Exp -> Compile JsExp
compileApp exp1 exp2 = do
   flattenApps <- config configFlattenApps
   if flattenApps then method2 else method1
   where
  -- Method 1:
  -- In this approach code ends up looking like this:
  -- a(a(a(a(a(a(a(a(a(a(L)(c))(b))(0))(0))(y))(t))(a(a(F)(3*a(a(d)+a(a(f)/20))))*a(a(f)/2)))(140+a(f)))(y))(t)})
  -- Which might be OK for speed, but increases the JS stack a fair bit.
  method1 =
    JsApp <$> (forceFlatName <$> compileExp exp1)
          <*> fmap return (compileExp exp2)
  forceFlatName name = JsApp (JsName JsForce) [name]

  -- Method 2:
  -- In this approach code ends up looking like this:
  -- d(O,a,b,0,0,B,w,e(d(I,3*e(e(c)+e(e(g)/20))))*e(e(g)/2),140+e(g),B,w)}),d(K,g,e(c)+0.05))
  -- Which should be much better for the stack and readability, but probably not great for speed.
  method2 = fmap flatten $
    JsApp <$> compileExp exp1
          <*> fmap return (compileExp exp2)
  flatten (JsApp op args) =
   case op of
     JsApp l r -> JsApp l (r ++ args)
     _        -> JsApp (JsName JsApply) (op : args)
  flatten x = x

-- | Compile a negate application
compileNegApp :: Exp -> Compile JsExp
compileNegApp e = JsNegApp . force <$> compileExp e

-- | Compile an infix application, optimizing the JS cases.
compileInfixApp :: Exp -> QOp -> Exp -> Compile JsExp
compileInfixApp exp1 ap exp2 = do
  qname <- resolveName op
  case qname of
    -- We can optimize prim ops. :-)
    Qual "Fay$" _
      | prettyPrint ap `elem` words "* + - / < > || &&" -> do
        e1 <- compileExp exp1
        e2 <- compileExp exp2
        fn <- compileExp (Var op)
        return $ JsApp (JsApp (force fn) [force e1]) [force e2]
    _ -> compileExp (App (App (Var op) exp1) exp2)

  where op = getOp ap
        getOp (QVarOp op) = op
        getOp (QConOp op) = op

-- | Compile a list expression.
compileList :: [Exp] -> Compile JsExp
compileList xs = do
  exps <- mapM compileExp xs
  return (makeList exps)

makeList :: [JsExp] -> JsExp
makeList exps = (JsApp (JsName (JsBuiltIn "list")) [JsList exps])

-- | Compile an if.
compileIf :: Exp -> Exp -> Exp -> Compile JsExp
compileIf cond conseq alt =
  JsTernaryIf <$> fmap force (compileExp cond)
              <*> compileExp conseq
              <*> compileExp alt

-- | Compile a lambda.
compileLambda :: [Pat] -> Exp -> Compile JsExp
compileLambda pats exp = do
  withScope $ do
    generateScope $ generateStatements JsNull
    exp <- compileExp exp
    stmts <- generateStatements exp
    case stmts of
      [JsEarlyReturn fun@JsFun{}] -> return fun
      _ -> error "Unexpected statements in compileLambda"

  where unhandledcase = throw "unhandled case" . JsName
        allfree = all isWildCardPat pats
        generateStatements exp =
          foldM (\inner (param,pat) -> do
                  stmts <- compilePat (JsName param) pat inner
                  return [JsEarlyReturn (JsFun [param] (stmts ++ [unhandledcase param | not allfree]) Nothing)])
                [JsEarlyReturn exp]
                (reverse (zip uniqueNames pats))

-- | Desugar list comprehensions.
desugarListComp :: Exp -> [QualStmt] -> Compile Exp
desugarListComp e [] =
    return (List [ e ])
desugarListComp e (QualStmt (Generator loc p e2) : stmts) = do
    nested <- desugarListComp e stmts
    withScopedTmpName $ \f ->
      return (Let (BDecls [ FunBind [
          Match loc f [ p         ] Nothing (UnGuardedRhs nested)    (BDecls []),
          Match loc f [ PWildCard ] Nothing (UnGuardedRhs (List [])) (BDecls [])
          ]]) (App (App (Var (UnQual (Ident "concatMap"))) (Var (UnQual f))) e2))
desugarListComp e (QualStmt (Qualifier e2)       : stmts) = do
    nested <- desugarListComp e stmts
    return (If e2 nested (List []))
desugarListComp e (QualStmt (LetStmt bs)         : stmts) = do
    nested <- desugarListComp e stmts
    return (Let bs nested)
desugarListComp _ (s                             : _    ) =
    throwError (UnsupportedQualStmt s)

-- | Desugar left sections to lambdas.
desugarLeftSection :: Exp -> QOp -> Compile Exp
desugarLeftSection e o = withScopedTmpName $ \tmp ->
    return (Lambda undefined [PVar tmp] (InfixApp e o (Var (UnQual tmp))))

-- | Desugar left sections to lambdas.
desugarRightSection :: QOp -> Exp -> Compile Exp
desugarRightSection o e = withScopedTmpName $ \tmp ->
    return (Lambda undefined [PVar tmp] (InfixApp (Var (UnQual tmp)) o e))

-- | Compile case expressions.
compileCase :: Exp -> [Alt] -> Compile JsExp
compileCase exp alts = do
  exp <- compileExp exp
  withScopedTmpJsName $ \tmpName -> do
    pats <- fmap optimizePatConditions $ mapM (compilePatAlt (JsName tmpName)) alts
    return $
      JsApp (JsFun [tmpName]
                   (concat pats)
                   (if any isWildCardAlt alts
                       then Nothing
                       else Just (throwExp "unhandled case" (JsName tmpName))))
            [exp]

-- | Compile a do block.
compileDoBlock :: [Stmt] -> Compile JsExp
compileDoBlock stmts = do
  doblock <- foldM compileStmt Nothing (reverse stmts)
  maybe (throwError EmptyDoBlock) compileExp doblock

-- | Compile a statement of a do block.
compileStmt :: Maybe Exp -> Stmt -> Compile (Maybe Exp)
compileStmt inner stmt =
  case inner of
    Nothing -> initStmt
    Just inner -> subsequentStmt inner

  where initStmt =
          case stmt of
            Qualifier exp -> return (Just exp)
            LetStmt{}     -> throwError LetUnsupported
            _             -> throwError InvalidDoBlock

        subsequentStmt inner =
          case stmt of
            Generator loc pat exp -> compileGenerator loc pat inner exp
            Qualifier exp -> return (Just (InfixApp exp
                                                    (QVarOp (UnQual (Symbol ">>")))
                                                    inner))
            LetStmt (BDecls binds) -> return (Just (Let (BDecls binds) inner))
            LetStmt _ -> throwError LetUnsupported
            RecStmt{} -> throwError RecursiveDoUnsupported

        compileGenerator srcloc pat inner exp = do
          let body = Lambda srcloc [pat] inner
          return (Just (InfixApp exp
                                 (QVarOp (UnQual (Symbol ">>=")))
                                 body))

-- | Compile the given pattern against the given expression.
compilePatAlt :: JsExp -> Alt -> Compile [JsStmt]
compilePatAlt exp alt@(Alt _ pat rhs wheres) = case wheres of
  BDecls (_ : _) -> throwError (UnsupportedWhereInAlt alt)
  IPBinds (_ : _) -> throwError (UnsupportedWhereInAlt alt)
  _ -> withScope $ do
    generateScope $ compilePat exp pat []
    alt <- compileGuardedAlt rhs
    compilePat exp pat [alt]

-- | Compile the given pattern against the given expression.
compilePat :: JsExp -> Pat -> [JsStmt] -> Compile [JsStmt]
compilePat exp pat body =
  case pat of
    PVar name       -> compilePVar name exp body
    PApp cons pats  -> compilePApp cons pats exp body
    PLit literal    -> compilePLit exp literal body
    PParen pat      -> compilePat exp pat body
    PWildCard       -> return body
    pat@PInfixApp{} -> compileInfixPat exp pat body
    PList pats      -> compilePList pats body exp
    PTuple pats     -> compilePList pats body exp
    PAsPat name pat -> compilePAsPat exp name pat body
    PRec name pats  -> compilePatFields exp name pats body
    pat             -> throwError (UnsupportedPattern pat)

-- | Compile a pattern variable e.g. x.
compilePVar :: Name -> JsExp -> [JsStmt] -> Compile [JsStmt]
compilePVar name exp body = do
  bindVar name
  return $ JsVar (JsNameVar (UnQual name)) exp : body

-- | Compile a record field pattern.
compilePatFields :: JsExp -> QName -> [PatField] -> [JsStmt] -> Compile [JsStmt]
compilePatFields exp name pats body = do
    c <- liftM (++ body) (compilePats' [] pats)
    qname <- resolveName name
    return [JsIf (force exp `JsInstanceOf` JsConstructor qname) c []]
  where -- compilePats' collects field names that had already been matched so that
        -- wildcard generates code for the rest of the fields.
        compilePats' :: [QName] -> [PatField] -> Compile [JsStmt]
        compilePats' names (PFieldPun name:xs) =
          compilePats' names (PFieldPat (UnQual name) (PVar name):xs)

        compilePats' names (PFieldPat fieldname (PVar varName):xs) = do
          r <- compilePats' (fieldname : names) xs
          bindVar varName
          return $ JsVar (JsNameVar (UnQual varName))
                         (JsGetProp (force exp) (JsNameVar fieldname))
                   : r -- TODO: think about this force call

        compilePats' names (PFieldWildcard:xs) = do
          records <- liftM stateRecords get
          let fields = fromJust (lookup name records)
              fields' = fields \\ names
          f <- mapM (\fieldName -> do bindVar (unQual fieldName)
                                      return (JsVar (JsNameVar fieldName)
                                             (JsGetProp (force exp) (JsNameVar fieldName))))
                   fields'
          r <- compilePats' names xs
          return $ f ++ r

        compilePats' _ [] = return []

        compilePats' _ (pat:_) = throwError (UnsupportedFieldPattern pat)

        unQual (Qual _ n) = n
        unQual (UnQual n) = n
        unQual Special{} = error "Trying to unqualify a Special..."

-- | Compile a literal value from a pattern match.
compilePLit :: JsExp -> Literal -> [JsStmt] -> Compile [JsStmt]
compilePLit exp literal body = do
  lit <- compileLit literal
  return [JsIf (equalExps exp lit)
               body
               []]

  where -- Equality test for two expressions, with some optimizations.
        equalExps :: JsExp -> JsExp -> JsExp
        equalExps a b
          | isConstant a && isConstant b = JsEq a b
          | isConstant a = JsEq a (force b)
          | isConstant b = JsEq (force a) b
          | otherwise =
             JsApp (JsName (JsBuiltIn "equal")) [a,b]

-- | Compile as binding in pattern match
compilePAsPat :: JsExp -> Name -> Pat -> [JsStmt] -> Compile [JsStmt]
compilePAsPat exp name pat body = do
  bindVar name
  x <- compilePat exp pat body
  return ([JsVar (JsNameVar (UnQual name)) exp] ++ x)

-- | Compile a record construction with named fields
-- | GHC will warn on uninitialized fields, they will be undefined in JS.
compileRecConstr :: QName -> [FieldUpdate] -> Compile JsExp
compileRecConstr name fieldUpdates = do
    -- var obj = new $_Type()
    qname <- resolveName name
    let record = JsVar (JsNameVar name) (JsNew (JsConstructor qname) [])
    setFields <- liftM concat (forM fieldUpdates (updateStmt name))
    return $ JsApp (JsFun [] (record:setFields) (Just (JsName (JsNameVar name)))) []
  where updateStmt :: QName -> FieldUpdate -> Compile [JsStmt]
        updateStmt o (FieldUpdate field value) = do
          exp <- compileExp value
          return [JsSetProp (JsNameVar o) (JsNameVar field) exp]
        updateStmt name FieldWildcard = do
          records <- liftM stateRecords get
          let fields = fromJust (lookup name records)
          return (map (\fieldName -> JsSetProp (JsNameVar name)
                                               (JsNameVar fieldName)
                                               (JsName (JsNameVar fieldName)))
                      fields)
        -- TODO: FieldPun
        -- I couldn't find a code that generates (FieldUpdate (FieldPun ..))
        updateStmt _ u = error ("updateStmt: " ++ show u)

updateRec :: Exp -> [FieldUpdate] -> Compile JsExp
updateRec rec fieldUpdates = do
    record <- force <$> compileExp rec
    let copyName = UnQual (Ident "$_record_to_update")
        copy = JsVar (JsNameVar copyName)
                     (JsRawExp ("Object.create(" ++ printJSString record ++ ")"))
    setFields <- forM fieldUpdates (updateExp copyName)
    return $ JsApp (JsFun [] (copy:setFields) (Just (JsName (JsNameVar copyName)))) []
  where updateExp :: QName -> FieldUpdate -> Compile JsStmt
        updateExp copyName (FieldUpdate field value) =
          JsSetProp (JsNameVar copyName) (JsNameVar field) <$> compileExp value
        updateExp copyName (FieldPun name) =
          -- let a = 1 in C {a}
          return $ JsSetProp (JsNameVar copyName)
                             (JsNameVar (UnQual name))
                             (JsName (JsNameVar (UnQual name)))
        -- TODO: FieldWildcard
        -- I also couldn't find a code that generates (FieldUpdate FieldWildCard)
        updateExp _ FieldWildcard = error "unsupported update: FieldWildcard"

-- | Compile a pattern application.
compilePApp :: QName -> [Pat] -> JsExp -> [JsStmt] -> Compile [JsStmt]
compilePApp cons pats exp body = do
  let forcedExp = force exp
  let boolIf b = return [JsIf (JsEq forcedExp (JsLit (JsBool b))) body []]
  case cons of
    -- Special-casing on the booleans.
    "True" -> boolIf True
    "False" -> boolIf False
    -- Everything else, generic:
    _ -> do
      rf <- fmap (lookup cons) (gets stateRecords)
      let recordFields =
            fromMaybe
              (error $ "Constructor '" ++ prettyPrint cons ++
                       "' was not found in stateRecords, did you try running this through GHC first?")
              rf
      substmts <- foldM (\body (field,pat) ->
                             compilePat (JsGetProp forcedExp (JsNameVar field)) pat body)
                  body
                  (reverse (zip recordFields pats))
      qcons <- resolveName cons
      return [JsIf (forcedExp `JsInstanceOf` JsConstructor qcons)
                   substmts
                   []]

-- | Compile a pattern list.
compilePList :: [Pat] -> [JsStmt] -> JsExp -> Compile [JsStmt]
compilePList [] body exp =
  return [JsIf (JsEq (force exp) JsNull) body []]
compilePList pats body exp = do
  let forcedExp = force exp
  stmts <- foldM (\body (i,pat) -> compilePat (JsApp (JsName (JsBuiltIn "index"))
                                                     [JsLit (JsInt i),forcedExp])
                                              pat
                                              body)
        body
        (reverse (zip [0..] pats))
  let patsLen = JsLit (JsInt (length pats))
  return [JsIf (JsApp (JsName (JsBuiltIn "listLen")) [forcedExp,patsLen])
               stmts
               []]

-- | Compile an infix pattern (e.g. cons and tuples.)
compileInfixPat :: JsExp -> Pat -> [JsStmt] -> Compile [JsStmt]
compileInfixPat exp pat@(PInfixApp left (Special cons) right) body =
  case cons of
    Cons -> do
      withScopedTmpJsName $ \tmpName -> do
        let forcedExp = JsName tmpName
            x = JsGetProp forcedExp (JsNameVar "car")
            xs = JsGetProp forcedExp (JsNameVar "cdr")
        rightMatch <- compilePat xs right body
        leftMatch <- compilePat x left rightMatch
        return [JsVar tmpName (force exp)
               ,JsIf (JsInstanceOf forcedExp (JsBuiltIn "Cons"))
                     leftMatch
                     []]
    _ -> throwError (UnsupportedPattern pat)
compileInfixPat _ pat _ = throwError (UnsupportedPattern pat)

-- | Compile a guarded alt.
compileGuardedAlt :: GuardedAlts -> Compile JsStmt
compileGuardedAlt alt =
  case alt of
    UnGuardedAlt exp -> JsEarlyReturn <$> compileExp exp
    GuardedAlts alts -> compileGuards (map altToRhs alts)
   where
    altToRhs (GuardedAlt l s e) = GuardedRhs l s e

-- | Compile a let expression.
compileLet :: [Decl] -> Exp -> Compile JsExp
compileLet decls exp = do
  withScope $ do
    generateScope $ mapM compileLetDecl decls
    binds <- mapM compileLetDecl decls
    body <- compileExp exp
    return (JsApp (JsFun [] (concat binds) (Just body)) [])

-- | Compile let declaration.
compileLetDecl :: Decl -> Compile [JsStmt]
compileLetDecl decl = do
  v <- case decl of
    decl@PatBind{} -> compileDecls False [decl]
    decl@FunBind{} -> compileDecls False [decl]
    TypeSig{}      -> return []
    _              -> throwError (UnsupportedLetBinding decl)
  return v

-- | Compile Haskell literal.
compileLit :: Literal -> Compile JsExp
compileLit lit =
  case lit of
    Char ch       -> return (JsLit (JsChar ch))
    Int integer   -> return (JsLit (JsInt (fromIntegral integer))) -- FIXME:
    Frac rational -> return (JsLit (JsFloating (fromRational rational)))
    -- TODO: Use real JS strings instead of array, probably it will
    -- lead to the same result.
    String string -> return (JsApp (JsName (JsBuiltIn "list"))
                                   [JsLit (JsStr string)])
    lit           -> throwError (UnsupportedLiteral lit)

-- | Maximum number of elements to allow in strict list representation
-- of arithmetic sequences.
maxStrictASLen :: Int
maxStrictASLen = 10

-- | Compile [e1..] arithmetic sequences.
compileEnumFrom :: Exp -> Compile JsExp
compileEnumFrom i = do
  e <- compileExp i
  name <- resolveName "enumFrom"
  return (JsApp (JsName (JsNameVar name)) [e])

-- | Compile [e1..e3] arithmetic sequences.
compileEnumFromTo :: Exp -> Exp -> Compile JsExp
compileEnumFromTo i i' = do
  f <- compileExp i
  t <- compileExp i'
  name <- resolveName "enumFromTo"
  cfg <- config id
  return $ case optEnumFromTo cfg f t of
    Just s -> s
    _ -> JsApp (JsApp (JsName (JsNameVar name)) [f]) [t]

-- | Optimize short literal [e1..e3] arithmetic sequences.
optEnumFromTo :: CompileConfig -> JsExp -> JsExp -> Maybe JsExp
optEnumFromTo cfg (JsLit f) (JsLit t) =
  if configOptimize cfg
  then case (f,t) of
    (JsInt fl, JsInt tl) -> strict JsInt fl tl
    (JsFloating fl, JsFloating tl) -> strict JsFloating fl tl
    _ -> Nothing
  else Nothing
    where strict :: (Enum a, Ord a, Num a) => (a -> JsLit) -> a -> a -> Maybe JsExp
          strict litfn f t =
            if fromEnum t - fromEnum f < maxStrictASLen
            then Just . makeList . map (JsLit . litfn) $ enumFromTo f t
            else Nothing
optEnumFromTo _ _ _ = Nothing

-- | Compile [e1,e2..] arithmetic sequences.
compileEnumFromThen :: Exp -> Exp -> Compile JsExp
compileEnumFromThen a b = do
  fr <- compileExp a
  th <- compileExp b
  name <- resolveName "enumFromThen"
  return (JsApp (JsApp (JsName (JsNameVar name)) [fr]) [th])

-- | Compile [e1,e2..e3] arithmetic sequences.
compileEnumFromThenTo :: Exp -> Exp -> Exp -> Compile JsExp
compileEnumFromThenTo a b z = do
  fr <- compileExp a
  th <- compileExp b
  to <- compileExp z
  name <- resolveName "enumFromThenTo"
  cfg <- config id
  return $ case optEnumFromThenTo cfg fr th to of
    Just s -> s
    _ -> JsApp (JsApp (JsApp (JsName (JsNameVar name)) [fr]) [th]) [to]

-- | Optimize short literal [e1,e2..e3] arithmetic sequences.
optEnumFromThenTo :: CompileConfig -> JsExp -> JsExp -> JsExp -> Maybe JsExp
optEnumFromThenTo cfg (JsLit fr) (JsLit th) (JsLit to) =
  if configOptimize cfg
  then case (fr,th,to) of
    (JsInt frl, JsInt thl, JsInt tol) -> strict JsInt frl thl tol
    (JsFloating frl, JsFloating thl, JsFloating tol) -> strict JsFloating frl thl tol
    _ -> Nothing
  else Nothing
    where strict :: (Enum a, Ord a, Num a) => (a -> JsLit) -> a -> a -> a -> Maybe JsExp
          strict litfn fr th to =
            if (fromEnum to - fromEnum fr) `div`
               (fromEnum th - fromEnum fr) + 1 < maxStrictASLen
            then Just . makeList . map (JsLit . litfn) $ enumFromThenTo fr th to
            else Nothing
optEnumFromThenTo _ _ _ _ = Nothing
