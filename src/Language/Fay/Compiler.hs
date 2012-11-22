{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
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
import           Language.Fay.Compiler.Optimizer
import           Language.Fay.Compiler.Misc
import           Language.Fay.Print         (printJSString)
import           Language.Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.IO
import           Control.Monad.State
import           Data.Default               (def)
import           Data.List
import           Data.List.Extra
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe
import           Language.Haskell.Exts
import           System.Directory           (doesFileExist)
import           System.FilePath            ((</>))
import           System.IO
import           System.Process.Extra

--------------------------------------------------------------------------------
-- Top level entry points

-- | Run the compiler.
runCompile :: CompileState -> Compile a -> IO (Either CompileError (a,CompileState))
runCompile state m = runErrorT (runStateT (unCompile m) state) where

-- | Compile a Haskell source string to a JavaScript source string.
compileViaStr :: (Show from,Show to,CompilesTo from to)
              => FilePath
              -> CompileConfig
              -> (from -> Compile to)
              -> String
              -> IO (Either CompileError (PrintState,CompileState))
compileViaStr filepath config with from = do
  cs <- defaultCompileState config
  runCompile (cs { stateFilePath = filepath })
             (parseResult (throwError . uncurry ParseError)
                          (fmap (\x -> execState (runPrinter (printJS x)) printConfig) . with)
                          (parseFay filepath from))

  where printConfig = def { psPretty = configPrettyPrint config }

-- | Compile a Haskell source string to a JavaScript source string.
compileToAst :: (Show from,Show to,CompilesTo from to)
              => FilePath
              -> CompileState
              -> (from -> Compile to)
              -> String
              -> IO (Either CompileError (to,CompileState))
compileToAst filepath state with from =
  runCompile state
             (parseResult (throwError . uncurry ParseError)
                          with
                          (parseFay filepath from))

-- | Parse some Fay code.
parseFay :: Parseable ast => FilePath -> String -> ParseResult ast
parseFay filepath = parseWithMode parseMode { parseFilename = filepath }

-- | The parse mode for Fay.
parseMode :: ParseMode
parseMode = defaultParseMode { extensions =
  [GADTs,StandaloneDeriving,EmptyDataDecls,TypeOperators,RecordWildCards,NamedFieldPuns] }

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
printTestCompile = printCompile def { configWarn = False,
  configDirectoryIncludes = [] } compileModule

-- | Compile the given Fay code for the documentation. This is
-- specialised because the documentation isn't really “real”
-- compilation.
compileForDocs :: Module -> Compile [JsStmt]
compileForDocs mod = do
  initialPass mod
  compileModule mod

-- | Compile the top-level Fay module.
compileToplevelModule :: Module -> Compile [JsStmt]
compileToplevelModule mod@(Module _ (ModuleName modulename) _ _ _ _ _)  = do
  cfg <- gets stateConfig
  when (configTypecheck cfg) $
    typecheck (configDirectoryIncludes cfg) [] (configWall cfg) $
      fromMaybe modulename $ configFilePath cfg
  initialPass mod
  cs <- liftIO $ defaultCompileState def
  modify $ \s -> s { stateImported = stateImported cs }
  stmts <- compileModule mod
  fay2js <- do syms <- gets stateFayToJs
               return $ if null syms then [] else [fayToJsDispatcher syms]
  js2fay <- do syms <- gets stateJsToFay
               return $ if null syms then [] else [jsToFayDispatcher syms]
  let maybeOptimize = if configOptimize cfg then runOptimizer optimizeToplevel else id
  return (maybeOptimize (stmts ++ fay2js ++ js2fay))

--------------------------------------------------------------------------------
-- Initial pass-through collecting record definitions

initialPass :: Module -> Compile ()
initialPass (Module _ _ _ Nothing _ imports decls) = do
  mapM_ initialPass_import (map translateModuleName imports)
  mapM_ (initialPass_decl True) decls

initialPass mod = throwError (UnsupportedModuleSyntax mod)

initialPass_import :: ImportDecl -> Compile ()
initialPass_import (ImportDecl _ "Prelude" _ _ _ _ _) = return ()
initialPass_import (ImportDecl _ name False _ Nothing Nothing Nothing) = do
  void $ unlessImported name $ \filepath contents -> do
    state <- gets id
    result <- liftIO $ initialPass_records filepath state initialPass contents
    case result of
      Right ((),st) -> do
        -- Merges the state gotten from passing through an imported
        -- module with the current state. We can assume no duplicate
        -- records exist since GHC would pick that up.
        modify $ \s -> s { stateRecords = stateRecords st
                         , stateImported = stateImported st
                         }
      Left err -> throwError err
    return []

initialPass_import i = throwError $ UnsupportedImport i

initialPass_records :: (Show from,Parseable from)
                    => FilePath
                    -> CompileState
                    -> (from -> Compile ())
                    -> String
                    -> IO (Either CompileError ((),CompileState))
initialPass_records filepath compileState with from =
  runCompile compileState
             (parseResult (throwError . uncurry ParseError)
                          with
                          (parseFay filepath from))

initialPass_decl :: Bool -> Decl -> Compile ()
initialPass_decl toplevel decl =
  case decl of
    DataDecl _ DataType _ _ _ constructors _ -> initialPass_dataDecl toplevel decl constructors
    GDataDecl _ DataType _l _i _v _n decls _ -> initialPass_dataDecl toplevel decl (map convertGADT decls)
    _ -> return ()

-- | Collect record definitions and store record name and field names.
-- A ConDecl will have fields named slot1..slotN
initialPass_dataDecl :: Bool -> Decl -> [QualConDecl] -> Compile ()
initialPass_dataDecl _ _decl constructors =
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

typecheck :: [FilePath] -> [String] -> Bool -> String -> Compile ()
typecheck includeDirs ghcFlags wall fp = do
  res <- liftIO $ readAllFromProcess' "ghc" (
    ["-fno-code", "-package fay", "-XNoImplicitPrelude", "-main-is", "Language.Fay.DummyMain", fp]
    ++ map ("-i" ++) includeDirs ++ ghcFlags ++ wallF) ""
  either error (warn . fst) res
   where
    wallF | wall = ["-Wall"]
          | otherwise = []

--------------------------------------------------------------------------------
-- Compilers

-- | Compile Haskell module.
compileModule :: Module -> Compile [JsStmt]
compileModule (Module _ modulename _pragmas Nothing exports imports decls) = do
  modify $ \s -> s { stateModuleName = modulename
                   , stateExportAll = isNothing exports
                   , stateExports = []
                   }
  imported <- fmap concat (mapM (compileImport . translateModuleName) imports)
  current <- compileDecls True decls
  -- If an export list is given we populate it beforehand,
  -- if not then bindToplevel will export each declaration when it's visited.
  mapM_ emitExport (fromMaybe [] exports)
  return (imported ++ current)
compileModule mod = throwError (UnsupportedModuleSyntax mod)

translateModuleName :: ImportDecl -> ImportDecl
-- The *.Prelude module doesn't contain actual code, but code to
-- appease GHC. The real code is in Stdlib, which could also be
-- imported directly, but it seems nicer to use Prelude. And maybe we
-- can fix this in the future so that Prelude contains the real
-- code. Doubt it, but it could happen.
translateModuleName (ImportDecl a (ModuleName "Language.Fay.Prelude") b c d e f) =
  (ImportDecl a (ModuleName "Language.Fay.Stdlib") b c d e f)
translateModuleName x = x

warn :: String -> Compile ()
warn "" = return ()
warn w = do
  shouldWarn <- configWarn <$> gets stateConfig
  when shouldWarn . liftIO . hPutStrLn stderr $ "Warning: " ++ w

instance CompilesTo Module [JsStmt] where compileTo = compileModule

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
    | otherwise = id

-- | Compile the given import.
compileImport :: ImportDecl -> Compile [JsStmt]
compileImport (ImportDecl _ "Prelude" _ _ _ _ _) = return []
compileImport (ImportDecl _ name False _ Nothing Nothing Nothing) = do
  unlessImported name $ \filepath contents -> do
    state <- gets id
    result <- liftIO $ compileToAst filepath state compileModule contents
    case result of
      Right (stmts,state) -> do
        modify $ \s -> s { stateFayToJs  = stateFayToJs state
                         , stateJsToFay  = stateJsToFay state
                         , stateImported = stateImported state
                         , stateScope    = mergeScopes (addExportsToScope (stateExports state) (stateScope s))
                                                       (stateScope state)
                         }
        return stmts
      Left err -> throwError err
compileImport i = throwError $ UnsupportedImport i

-- | Add the new scopes to the old one, stripping out local bindings.
mergeScopes :: Map Name [NameScope] -> Map Name [NameScope] -> Map Name [NameScope]
mergeScopes old new =
  M.map (filter (/=ScopeBinding))
        (foldr (\(key,val) -> M.insertWith (++) key val) old (M.assocs new))

-- | Add
addExportsToScope :: [QName] -> Map Name [NameScope] -> Map Name [NameScope]
addExportsToScope exports mapping = foldr copy mapping exports where
  copy e =
    case e of
      Qual modname name -> M.insertWith (++) name [ScopeImported modname Nothing]
      UnQual name       -> error $ "Exports should not be unqualified: " ++ prettyPrint name
      Special{}         -> error $ "Don't be silly."

-- | Don't re-import the same modules.
unlessImported :: ModuleName -> (FilePath -> String -> Compile [JsStmt]) -> Compile [JsStmt]
unlessImported name importIt = do
  imported <- gets stateImported
  case lookup name imported of
    Just _ -> return []
    Nothing -> do
      dirs <- configDirectoryIncludes <$> gets stateConfig
      (filepath,contents) <- findImport dirs name
      modify $ \s -> s { stateImported = (name,filepath) : imported }
      importIt filepath contents

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
  bindVar ident
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
          return [cons, func]
        InfixConDecl t1 name t2 -> do
          let slots = ["slot1","slot2"]
              fields = zip (map return slots) [t1, t2]
          cons <- makeConstructor name slots
          func <- makeFunc name slots
          emitFayToJs name fields
          emitJsToFay name fields
          return [cons, func]
        RecDecl name fields' -> do
          let fields = concatMap fst fields'
          cons <- makeConstructor name fields
          func <- makeFunc name fields
          funs <- makeAccessors srcloc fields
          emitFayToJs name fields'
          emitJsToFay name fields'
          return (cons : func : funs)

  where
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
            generateScope $ mapM (\(arg,pat) -> compilePat (JsName arg) pat []) (zip args pats)
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
    EnumFrom i                    -> do e <- compileExp i
                                        name <- resolveName "enumFrom"
                                        return (JsApp (JsName (JsNameVar name)) [e])
    EnumFromTo i i'               -> do f <- compileExp i
                                        t <- compileExp i'
                                        name <- resolveName "enumFromTo"
                                        return (JsApp (JsApp (JsName (JsNameVar name))
                                                             [f])
                                                      [t])
    EnumFromThen a b              -> do fr <- compileExp a
                                        th <- compileExp b
                                        name <- resolveName "enumFromThen"
                                        return (JsApp (JsApp (JsName (JsNameVar name))
                                                             [fr])
                                                      [th])
    EnumFromThenTo a b z          -> do fr <- compileExp a
                                        th <- compileExp b
                                        to <- compileExp z
                                        name <- resolveName "enumFromThenTo"
                                        return (JsApp (JsApp (JsApp (JsName (JsNameVar name))
                                                                    [fr])
                                                             [th])
                                                      [to])
    RecConstr name fieldUpdates -> compileRecConstr name fieldUpdates
    RecUpdate rec  fieldUpdates -> updateRec rec fieldUpdates
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
    _ -> compileExp (App (App (Var qname) exp1) exp2)

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

-- | Compile list comprehensions.
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
compilePatAlt exp (Alt _ pat rhs _) = do
  withScope $ do
    generateScope $ compilePat exp pat []
    alt <- compileGuardedAlt rhs
    compilePat exp pat [JsEarlyReturn alt]

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
  return ([JsVar (JsNameVar (UnQual name)) exp] ++ x ++ body)

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
  stmts <- foldM (\body (i,pat) -> compilePat (JsApp (JsApp (JsName (JsBuiltIn "index"))
                                                   [JsLit (JsInt i)])
                                            [forcedExp])
                                     pat body)
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
compileGuardedAlt :: GuardedAlts -> Compile JsExp
compileGuardedAlt alt =
  case alt of
    UnGuardedAlt exp -> compileExp exp
    alt -> throwError (UnsupportedGuardedAlts alt)

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
