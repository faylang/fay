{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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

import           Language.Fay.Print          (jsEncodeName, printJSString)
import           Language.Fay.Types
import           Language.Fay.Compiler.FFI
import           Language.Fay.Compiler.Misc

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.IO
import           Control.Monad.State
import           Data.Default                (def)
import           Data.List
import           Data.Maybe
import           Data.String
import           Data.Word
import           Language.Haskell.Exts
import           System.Directory            (doesFileExist)
import           System.FilePath             ((</>))
import           System.IO
import           System.Process.Extra
import           System.Random

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
              -> IO (Either CompileError (String,CompileState))
compileViaStr filepath config with from =
  runCompile ((defaultCompileState config) { stateFilePath = filepath })
             (parseResult (throwError . uncurry ParseError)
                          (fmap printJSString . with)
                          (parseFay filepath from))

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
    Right (ok,_) -> putStr ok

-- | Compile a String of Fay and print it as beautified JavaScript.
printTestCompile :: String -> IO ()
printTestCompile = printCompile def { configWarn = False } compileModule

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
  modify $ \s -> s { stateImported = stateImported (defaultCompileState def) }
  stmts <- compileModule mod
  fay2js <- gets (fayToJsDispatcher . stateFayToJs)
  js2fay <- gets (jsToFayDispatcher . stateJsToFay)
  return (stmts ++ [fay2js,js2fay])

--------------------------------------------------------------------------------
-- Initial pass-through collecting record definitions

initialPass :: Module -> Compile ()
initialPass (Module _ _ _ Nothing _ imports decls) = do
  mapM_ initialPass_import imports
  mapM_ (initialPass_decl True) decls

initialPass mod = throwError (UnsupportedModuleSyntax mod)

initialPass_import :: ImportDecl -> Compile ()
initialPass_import (ImportDecl _ (ModuleName name) False _ Nothing Nothing Nothing) = do
  void $ unlessImported name $ do
    dirs <- configDirectoryIncludes <$> gets stateConfig
    (filepath,contents) <- findImport dirs name
    cs <- gets id
    result <- liftIO $ initialPass_records filepath cs initialPass contents
    case result of
      Right ((),state) -> do
        -- Merges the state gotten from passing through an imported
        -- module with the current state. We can assume no duplicate
        -- records exist since GHC would pick that up.
        modify $ \s -> s { stateRecords = stateRecords state
                         , stateImported = stateImported state
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
      ConDecl (UnQual -> name) types  -> do
        let fields =  map (Ident . ("slot"++) . show . fst) . zip [1 :: Integer ..] $ types
        addRecordState name fields
      InfixConDecl _t1 (UnQual -> name) _t2 ->
        addRecordState name ["slot1", "slot2"]
      RecDecl (UnQual -> name) fields' -> do
        let fields = concatMap fst fields'
        addRecordState name fields

  where
    addRecordState :: QName -> [Name] -> Compile ()
    addRecordState name fields = modify $ \s -> s { stateRecords = (Ident (qname name), fields) : stateRecords s }

--------------------------------------------------------------------------------
-- Typechecking

typecheck :: [FilePath] -> [String] -> Bool -> String -> Compile ()
typecheck includeDirs ghcFlags wall fp = do
  res <- liftIO $ readAllFromProcess' "ghc" (
    ["-fno-code", "-package fay", fp] ++ map ("-i" ++) includeDirs ++ ghcFlags ++ wallF) ""
  either error (warn . fst) res
  where
    wallF | wall = ["-Wall"]
          | otherwise = []

--------------------------------------------------------------------------------
-- Compilers

-- | Compile Haskell module.
compileModule :: Module -> Compile [JsStmt]
compileModule (Module _ modulename pragmas Nothing exports imports decls) = do
  checkModulePragmas pragmas
  modify $ \s -> s { stateModuleName = modulename
                   , stateExportAll = isNothing exports
                   }
  mapM_ emitExport (fromMaybe [] exports)
  imported <- fmap concat (mapM compileImport imports)
  current <- compileDecls True decls
  return (imported ++ current)
compileModule mod = throwError (UnsupportedModuleSyntax mod)

warn :: String -> Compile ()
warn "" = return ()
warn w = do
  shouldWarn <- configWarn <$> gets stateConfig
  when shouldWarn . liftIO . hPutStrLn stderr $ "Warning: " ++ w

checkModulePragmas :: [ModulePragma] -> Compile ()
checkModulePragmas pragmas =
  when (not $ any noImplicitPrelude pragmas) $
    warn "NoImplicitPrelude not specified"
  where
    noImplicitPrelude :: ModulePragma -> Bool
    noImplicitPrelude (LanguagePragma _ names) = any (== (Ident "NoImplicitPrelude")) names
    noImplicitPrelude _ = False

instance CompilesTo Module [JsStmt] where compileTo = compileModule

findImport :: [FilePath] -> String -> Compile (FilePath,String)
findImport alldirs = go alldirs where
  go (dir:dirs) name = do
    exists <- io (doesFileExist path)
    if exists
      then fmap (path,) (io (readFile path))
      else go dirs name
    where
      path = dir </> replace '.' '/' name ++ ".hs"
      replace c r = map (\x -> if x == c then r else x)
  go [] name =
    throwError $ Couldn'tFindImport name alldirs

-- | Compile the given import.
compileImport :: ImportDecl -> Compile [JsStmt]
compileImport (ImportDecl _ (ModuleName name) False _ Nothing Nothing Nothing) = do
  unlessImported name $ do
    dirs <- configDirectoryIncludes <$> gets stateConfig
    (filepath,contents) <- findImport dirs name
    state <- gets id
    result <- liftIO $ compileToAst filepath state compileModule contents
    case result of
      Right (stmts,state) -> do
        modify $ \s -> s { stateFayToJs = stateFayToJs state
                         , stateJsToFay = stateJsToFay state
                         , stateImported = stateImported state
                         }
        return stmts
      Left err -> throwError err
compileImport i = throwError $ UnsupportedImport i

-- | Don't re-import the same modules.
unlessImported :: String -> Compile [JsStmt] -> Compile [JsStmt]
unlessImported name importIt = do
  imported <- gets stateImported
  if elem name imported
     then return []
     else do
       modify $ \s -> s { stateImported = name : imported }
       importIt

-- | Compile Haskell declaration.
compileDecls :: Bool -> [Decl] -> Compile [JsStmt]
compileDecls toplevel decls =
  case decls of
    [] -> return []
    (TypeSig _ _ sig:bind@PatBind{}:decls) -> appendM (compilePatBind toplevel (Just sig) bind)
                                                      (compileDecls toplevel decls)
    (decl:decls) -> appendM (compileDecl toplevel decl)
                            (compileDecls toplevel decls)

  where appendM m n = do x <- m
                         xs <- n
                         return (x ++ xs)

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
    PatBind srcloc (PVar ident) Nothing (UnGuardedRhs rhs) bdecls ->
      compileUnguardedRhs srcloc toplevel ident (Let bdecls rhs)
    _ -> throwError (UnsupportedDeclaration pat)

  where ffiExp (App (Var (UnQual (Ident "ffi"))) (Lit (String formatstr))) = Just formatstr
        ffiExp _ = Nothing

-- | Compile a normal simple pattern binding.
compileUnguardedRhs :: SrcLoc -> Bool -> Name -> Exp -> Compile [JsStmt]
compileUnguardedRhs srcloc toplevel ident rhs = do
  body <- compileExp rhs
  bind <- bindToplevel srcloc toplevel (UnQual ident) (thunk body)
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
        ConDecl (UnQual -> name) types  -> do
          let fields =  map (Ident . ("slot"++) . show . fst) . zip [1 :: Integer ..] $ types
              fields' = (zip (map return fields) types)
          cons <- makeConstructor name fields
          func <- makeFunc name fields
          emitFayToJs name fields'
          emitJsToFay name fields'
          return [cons, func]
        InfixConDecl t1 (UnQual -> name) t2 -> do
          let slots = [Ident "slot1", Ident "slot2"]
              fields = zip (map return slots) [t1, t2]
          cons <- makeConstructor name slots
          func <- makeFunc name slots
          emitFayToJs name fields
          emitJsToFay name fields
          return [cons, func]
        RecDecl (UnQual -> name) fields' -> do
          let fields = concatMap fst fields'
          cons <- makeConstructor name fields
          func <- makeFunc name fields
          funs <- makeAccessors srcloc fields
          emitFayToJs name fields'
          emitJsToFay name fields'
          return (cons : func : funs)

  where
    -- Creates a constructor R_RecConstr for a Record
    makeConstructor name fields = do
          let fieldParams = map (fromString . unname) fields
          return $
            JsVar (constructorName name) $
              JsFun fieldParams
                  (flip map fields $ \field@(Ident s) ->
                     JsSetProp (fromString ":this") (UnQual field) (JsName (fromString s)))
                Nothing

    -- Creates a function to initialize the record by regular application
    makeFunc name fields = do
          let fieldParams = map (\(Ident s) -> fromString s) fields
          let fieldExps = map (JsName . UnQual) fields
          return $ JsVar name $
            foldr (\slot inner -> JsFun [slot] [] (Just inner))
              (thunk $ JsNew (constructorName name) fieldExps)
              fieldParams

    -- Creates getters for a RecDecl's values
    makeAccessors srcloc fields =
      forM fields $ \(Ident name) ->
           bindToplevel srcloc
                        toplevel
                        (fromString name)
                        (JsFun ["x"]
                               []
                               (Just (thunk (JsGetProp (force (JsName "x"))
                                                       (fromString name)))))

-- | Compile a function which pattern matches (causing a case analysis).
compileFunCase :: Bool -> [Match] -> Compile [JsStmt]
compileFunCase _toplevel [] = return []
compileFunCase toplevel matches@(Match srcloc name argslen _ _ _:_) = do
  tco  <- config configTCO
  pats <- fmap optimizePatConditions (mapM compileCase matches)
  bind <- bindToplevel srcloc
                       toplevel
                       (UnQual name)
                       (foldr (\arg inner -> JsFun [arg] [] (Just inner))
                              (stmtsThunk (let stmts = (concat pats ++ basecase)
                                           in if tco
                                                 then optimizeTailCalls args name stmts
                                                 else stmts))
                              args)
  return [bind]
  where args = zipWith const uniqueNames argslen

        isWildCardMatch (Match _ _ pats _ _ _) = all isWildCardPat pats

        compileCase :: Match -> Compile [JsStmt]
        compileCase match@(Match _ _ pats _ rhs _) = do
          whereDecls' <- whereDecls match
          exp  <- compileRhs rhs
          body <- if null whereDecls'
                    then return exp
                    else do
                        binds <- mapM compileLetDecl whereDecls'
                        return (JsApp (JsFun [] (concat binds) (Just exp)) [])
          foldM (\inner (arg,pat) ->
                  compilePat (JsName arg) pat inner)
                [JsEarlyReturn body]
                (zip args pats)

        whereDecls :: Match -> Compile [Decl]
        whereDecls (Match _ _ _ _ _ (BDecls decls)) = return decls
        whereDecls match = throwError (UnsupportedWhereInMatch match)

        basecase :: [JsStmt]
        basecase = if any isWildCardMatch matches
                      then []
                      else [throw ("unhandled case in " ++ show name)
                                  (JsList (map JsName args))]

-- | Optimize functions in tail-call form.
optimizeTailCalls :: [JsParam] -- ^ The function parameters.
                  -> Name      -- ^ The function name.
                  -> [JsStmt]  -- ^ The body of the function.
                  -> [JsStmt]  -- ^ A new optimized function body.
optimizeTailCalls params name stmts = abandonIfNoChange $
  JsWhile (JsLit (JsBool True))
          (concatMap replaceTailStmt
                     (reverse (zip (reverse stmts) [0::Integer ..])))

  where replaceTailStmt (JsIf cond sothen orelse,i) = [JsIf cond (concatMap (replaceTailStmt . (,i)) sothen)
                                                                 (concatMap (replaceTailStmt . (,i)) orelse)]
        replaceTailStmt (JsEarlyReturn exp,i) = expTailReplace i exp
        replaceTailStmt (x,_) = [x]
        expTailReplace i (flatten -> Just (JsName (UnQual call):args@(_:_)))
          | call == name = updateParamsInstead i args
        expTailReplace _i original = [JsEarlyReturn original]
        updateParamsInstead i args = zipWith JsUpdate params args ++
                                     [JsContinue | i /= 0]
        abandonIfNoChange (JsWhile _ newstmts)
          | newstmts == stmts = stmts
        abandonIfNoChange new = [new]

-- | Flatten an application expression into function : arg : arg : []
flatten :: JsExp -> Maybe [JsExp]
flatten (JsApp op@JsApp{} arg) = do
  inner <- expand op
  return (inner ++ arg)
flatten name@JsName{} = return [name]
flatten _ = Nothing

-- | Expand a forced value into the value.
expand :: JsExp -> Maybe [JsExp]
expand (JsApp (JsName (UnQual (Ident "_"))) xs) =
  fmap concat (mapM flatten xs)
expand _ = Nothing

-- | Compile a right-hand-side expression.
compileRhs :: Rhs -> Compile JsExp
compileRhs (UnGuardedRhs exp) = compileExp exp
compileRhs (GuardedRhss rhss) = compileGuards rhss

-- | Compile guards
compileGuards :: [GuardedRhs] -> Compile JsExp
compileGuards [] = return . JsThrowExp . JsLit . JsStr $ "Non-exhaustive guards"
compileGuards ((GuardedRhs _ (Qualifier (Var (UnQual (Ident "otherwise"))):_) exp):_) = compileExp exp
compileGuards (GuardedRhs _ (Qualifier guard:_) exp : rest) =
  JsTernaryIf <$> fmap force (compileExp guard)
              <*> compileExp exp
              <*> compileGuards rest
compileGuards rhss = throwError . UnsupportedRhs . GuardedRhss $ rhss

-- | Compile Haskell expression.
compileExp :: Exp -> Compile JsExp
compileExp exp =
  case exp of
    Paren exp                     -> compileExp exp
-- Commented out: See #59
--    Var (UnQual (Ident "return")) -> return (JsName (hjIdent "return"))
    Var qname                     -> return (JsName qname)
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
    Con exp                       -> return (JsName exp)
    Do stmts                      -> compileDoBlock stmts
    Lambda _ pats exp             -> compileLambda pats exp
    EnumFrom i                    -> do e <- compileExp i
                                        return (JsApp (JsName "enumFrom") [e])
    EnumFromTo i i'               -> do f <- compileExp i
                                        t <- compileExp i'
                                        return (JsApp (JsApp (JsName "enumFromTo") [f])
                                                      [t])
    RecConstr name fieldUpdates -> compileRecConstr name fieldUpdates
    RecUpdate rec  fieldUpdates -> updateRec rec fieldUpdates
    ListComp exp stmts            -> compileExp =<< desugarListComp exp stmts
    ExpTypeSig _ e _ -> compileExp e

    exp -> throwError (UnsupportedExpression exp)

instance CompilesTo Exp JsExp where compileTo = compileExp

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
  forceFlatName name = JsApp (JsName "_") [name]

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
     _        -> JsApp (JsName "__") (op : args)
  flatten x = x

-- | Compile a negate application
compileNegApp :: Exp -> Compile JsExp
compileNegApp e = JsNegApp . force <$> compileExp e

-- | Compile an infix application, optimizing the JS cases.
compileInfixApp :: Exp -> QOp -> Exp -> Compile JsExp
compileInfixApp exp1 op exp2 = do
  case getOp op of
    UnQual (Symbol symbol)
      | symbol `elem` words "* + - / < > || &&" -> do
          e1 <- compileExp exp1
          e2 <- compileExp exp2
          fn <- resolveOpToVar >=> compileExp $ op
          return $ JsApp (JsApp (force fn) [(force e1)]) [(force e2)]
    _ -> do
      var <- resolveOpToVar op
      compileExp (App (App var exp1) exp2)

  where getOp (QVarOp op) = op
        getOp (QConOp op) = op

-- | Compile a list expression.
compileList :: [Exp] -> Compile JsExp
compileList xs = do
  exps <- mapM compileExp xs
  return (makeList exps)

makeList :: [JsExp] -> JsExp
makeList exps = (JsApp (JsName (hjIdent "list")) [JsList exps])

-- | Compile an if.
compileIf :: Exp -> Exp -> Exp -> Compile JsExp
compileIf cond conseq alt =
  JsTernaryIf <$> fmap force (compileExp cond)
              <*> compileExp conseq
              <*> compileExp alt

-- | Compile a lambda.
compileLambda :: [Pat] -> Exp -> Compile JsExp
compileLambda pats exp = do
  exp <- compileExp exp
  stmts <- foldM (\inner (param,pat) -> do
                   stmts <- compilePat (JsName param) pat inner
                   return [JsEarlyReturn (JsFun [param] (stmts ++ [unhandledcase param | not allfree]) Nothing)])
                 [JsEarlyReturn exp]
                 (reverse (zip uniqueNames pats))
  case stmts of
    [JsEarlyReturn fun@JsFun{}] -> return fun
    _ -> error "Unexpected statements in compileLambda"

  where unhandledcase = throw "unhandled case" . JsName
        allfree = all isWildCardPat pats

-- | This is a hack to get hopefully fresh variable names.  Really we should
-- keep some kind of counter, or even a map of encountered variable names to
-- uniqueify, but this has only a one in a billion chance of failure, and it
-- works for testing.
freshVariable :: Compile Name
freshVariable = do
  n <- liftIO $ randomRIO (1, maxBound :: Word64)
  return (Ident ("freshVar__" ++ show n))

-- | Compile list comprehensions.
desugarListComp :: Exp -> [QualStmt] -> Compile Exp
desugarListComp e [] =
    return (List [ e ])
desugarListComp e (QualStmt (Generator loc p e2) : stmts) = do
    nested <- desugarListComp e stmts
    f      <- freshVariable
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
  withScopedTmpName $ \tmpName -> do
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
  alt <- compileGuardedAlt rhs
  compilePat exp pat [JsEarlyReturn alt]

-- | Compile the given pattern against the given expression.
compilePat :: JsExp -> Pat -> [JsStmt] -> Compile [JsStmt]
compilePat exp pat body =
  case pat of
    PVar name       -> return $ JsVar (UnQual name) exp : body
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

-- | Compile a record field pattern.
compilePatFields :: JsExp -> QName -> [PatField] -> [JsStmt] -> Compile [JsStmt]
compilePatFields exp (Ident . qname -> name) pats body = do
    c <- liftM (++ body) (compilePats' [] pats)
    return [JsIf ((force exp) `JsInstanceOf` constructorName (UnQual name)) c []]
  where -- compilePats' collects field names that had already been matched so that
        -- wildcard generates code for the rest of the fields.
        compilePats' :: [Name] -> [PatField] -> Compile [JsStmt]
        compilePats' names (PFieldPun name:xs) =
          compilePats' names (PFieldPat (UnQual name) (PVar name):xs)

        compilePats' names (PFieldPat fieldQName (PVar varName):xs) = do
          let fieldname = case fieldQName of
                            (Qual _ name) -> name
                            (UnQual name) -> name
                            (Special _) -> undefined -- TODO: how to handle special names ?
          r <- compilePats' (fieldname:names) xs
          return $ JsVar (UnQual varName) (JsGetProp (force exp) fieldQName):r -- TODO: think about this force call

        compilePats' names (PFieldWildcard:xs) = do
          records <- liftM stateRecords get
          let fields = fromJust (lookup name records)
              fields' = fields \\ names
              f = map (\fieldName -> JsVar (UnQual fieldName) (JsGetProp (force exp) (UnQual fieldName))) fields'
          r <- compilePats' names xs
          return $ f ++ r

        compilePats' _ [] = return []

        compilePats' _ (pat:_) = throwError (UnsupportedFieldPattern pat)

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
             JsApp (JsName (hjIdent "equal")) [a,b]

-- | Compile as binding in pattern match
compilePAsPat :: JsExp -> Name -> Pat -> [JsStmt] -> Compile [JsStmt]
compilePAsPat exp name pat body = do
  x <- compilePat exp pat body
  return ([JsVar (UnQual name) exp] ++ x ++ body)

-- | Compile a record construction with named fields
-- | GHC will warn on uninitialized fields, they will be undefined in JS.
compileRecConstr :: QName -> [FieldUpdate] -> Compile JsExp
compileRecConstr name fieldUpdates = do
    let o = UnQual (Ident (qname name))
    -- var obj = new $_Type()
    let record = JsVar o (JsNew (constructorName name) [])
    setFields <- liftM concat (forM fieldUpdates (updateStmt o))
    return $ JsApp (JsFun [] (record:setFields) (Just (JsName o))) []
  where updateStmt :: QName -> FieldUpdate -> Compile [JsStmt]
        updateStmt o (FieldUpdate field value) = do
          exp <- compileExp value
          return [JsSetProp o field exp]
        updateStmt o@(UnQual name) FieldWildcard = do
          records <- liftM stateRecords get
          let fields = fromJust (lookup name records)
          return (map (\fieldName -> JsSetProp o (UnQual fieldName) (JsName (UnQual fieldName))) fields)
        -- TODO: FieldPun
        -- I couldn't find a code that generates (FieldUpdate (FieldPun ..))
        updateStmt _ u = error ("updateStmt: " ++ show u)

updateRec :: Exp -> [FieldUpdate] -> Compile JsExp
updateRec rec fieldUpdates = do
    record <- force <$> compileExp rec
    let copyName = UnQual (Ident "$_record_to_update")
        copy = JsVar copyName
                     (JsRawExp ("Object.create(" ++ printJSString record ++ ")"))
    setFields <- forM fieldUpdates (updateExp copyName)
    return $ JsApp (JsFun [] (copy:setFields) (Just (JsName copyName))) []
  where updateExp :: QName -> FieldUpdate -> Compile JsStmt
        updateExp copyName (FieldUpdate field value) =
          JsSetProp copyName field <$> compileExp value
        updateExp copyName (FieldPun name) =
          -- let a = 1 in C {a}
          return $ JsSetProp copyName (UnQual name) (JsName (UnQual name))
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
      rf <- lookup (Ident (qname cons)) <$> gets stateRecords
      let recordFields =
            fromMaybe
              (error $ "Constructor '" ++ qname cons ++
                       "' was not found in stateRecords, did you try running this through GHC first?")
              rf
      substmts <- foldM (\body (Ident field,pat) ->
                             compilePat (JsGetProp forcedExp (fromString field)) pat body)
                  body
                  (reverse (zip recordFields pats))
      return [JsIf (forcedExp `JsInstanceOf` constructorName cons)
                   substmts
                   []]

-- | Compile a pattern list.
compilePList :: [Pat] -> [JsStmt] -> JsExp -> Compile [JsStmt]
compilePList [] body exp =
  return [JsIf (JsEq (force exp) JsNull) body []]
compilePList pats body exp = do
  let forcedExp = force exp
  foldM (\body (i,pat) -> compilePat (JsApp (JsApp (JsName (hjIdent "index"))
                                                   [JsLit (JsInt i)])
                                            [forcedExp])
                                     pat body)
        body
        (reverse (zip [0..] pats))

-- | Compile an infix pattern (e.g. cons and tuples.)
compileInfixPat :: JsExp -> Pat -> [JsStmt] -> Compile [JsStmt]
compileInfixPat exp pat@(PInfixApp left (Special cons) right) body =
  case cons of
    Cons -> do
      withScopedTmpName $ \tmpName -> do
        let forcedExp = JsName tmpName
            x = JsGetProp forcedExp "car"
            xs = JsGetProp forcedExp "cdr"
        rightMatch <- compilePat xs right body
        leftMatch <- compilePat x left rightMatch
        return [JsVar tmpName (force exp)
               ,JsIf (JsInstanceOf forcedExp (hjIdent "Cons"))
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
  body <- compileExp exp
  binds <- mapM compileLetDecl decls
  return (JsApp (JsFun [] (concat binds) (Just body)) [])

-- | Compile let declaration.
compileLetDecl :: Decl -> Compile [JsStmt]
compileLetDecl decl =
  case decl of
    decl@PatBind{} -> compileDecls False [decl]
    decl@FunBind{} -> compileDecls False [decl]
    TypeSig{}      -> return []
    _              -> throwError (UnsupportedLetBinding decl)

-- | Compile Haskell literal.
compileLit :: Literal -> Compile JsExp
compileLit lit =
  case lit of
    Char ch       -> return (JsLit (JsChar ch))
    Int integer   -> return (JsLit (JsInt (fromIntegral integer))) -- FIXME:
    Frac rational -> return (JsLit (JsFloating (fromRational rational)))
    -- TODO: Use real JS strings instead of array, probably it will
    -- lead to the same result.
    String string -> return (JsApp (JsName (hjIdent "list"))
                                   [JsLit (JsStr string)])
    lit           -> throwError (UnsupportedLiteral lit)
