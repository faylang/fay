{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing -fno-warn-orphans #-}

-- | The Haskell→Javascript compiler.

module Language.Fay

  where

import           Language.Fay.Print         ()
import           Language.Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.IO
import           Control.Monad.State
import           Data.Char
import           Data.Default (def)
import           Data.List
import           Data.Maybe
import           Data.String
import           Language.Haskell.Exts
import           Safe

import qualified Language.JavaScript.Parser as JS
import           System.Process.Extra

--------------------------------------------------------------------------------
-- Top level entry points

-- | Compile something that compiles to something else.
compile :: CompilesTo from to => CompileConfig -> from -> IO (Either CompileError (to,CompileState))
compile config = runCompile config . compileTo

-- | Run the compiler.
runCompile :: CompileConfig -> Compile a -> IO (Either CompileError (a,CompileState))
runCompile config m = runErrorT (runStateT (unCompile m) state) where
  state = CompileState { stateConfig  = config
                       , stateExports = []
                       , stateModuleName = "Main"
                       , stateExportAll = True
                       }

-- | Compile a Haskell source string to a JavaScript source string.
compileViaStr :: (Show from,Show to,CompilesTo from to)
              => CompileConfig
              -> (from -> Compile to)
              -> String
              -> IO (Either CompileError (String,CompileState))
compileViaStr config with from =
  runCompile config
             (parseResult (throwError . uncurry ParseError)
                          (fmap printJS . with)
                          (parse from))

-- | Compile a Haskell source string to a JavaScript source string.
compileToAst :: (Show from,Show to,CompilesTo from to)
              => CompileConfig
              -> (from -> Compile to)
              -> String
              -> IO (Either CompileError (to,CompileState))
compileToAst config with from =
  runCompile config
             (parseResult (throwError . uncurry ParseError)
                          with
                          (parse from))

-- | Compile from a string.
compileFromStr :: (Parseable a, MonadError CompileError m) => (a -> m a1) -> String -> m a1
compileFromStr with from =
  parseResult (throwError . uncurry ParseError)
              (with)
              (parse from)

printCompile :: (Show from,Show to,CompilesTo from to)
              => CompileConfig
              -> (from -> Compile to)
              -> String
              -> IO ()
printCompile config with from = do
  result <- compileViaStr config with from
  case result of
    Left err -> putStrLn $ show err
    Right (ok,_) -> do writeFile "/tmp/x.js" ok
                       prettyPrintFile "/tmp/x.js" >>= putStr

-- | Compile a String of Fay and print it as beautified JavaScript.
printTestCompile :: String -> IO ()
printTestCompile = printCompile def compileModule


--------------------------------------------------------------------------------
-- Compilers

-- | Compile Haskell module.
compileModule :: Module -> Compile [JsStmt]
compileModule (Module _ modulename _pragmas Nothing exports imports decls) = do
  modify $ \s -> s { stateModuleName = modulename
                   , stateExportAll = isNothing exports
                   }
  mapM_ emitExport (fromMaybe [] exports)
  imported <- fmap concat (mapM compileImport imports)
  current <- compileDecls True decls
  return (imported ++ current)
compileModule mod = throwError (UnsupportedModuleSyntax mod)

instance CompilesTo Module [JsStmt] where compileTo = compileModule

-- | Compile the given import.
compileImport :: ImportDecl -> Compile [JsStmt]
compileImport (ImportDecl _ (ModuleName name) _ _ _ _ _)
  | elem name ["Language.Fay.Prelude","Language.Fay.FFI","Language.Fay.Types"] || name == "Prelude" = return []
compileImport (ImportDecl _ (ModuleName name) False _ Nothing Nothing Nothing) = do
  contents <- io (readFile (replace '.' '/' name ++ ".hs"))
  cfg <- config id
  result <- liftIO $ compileToAst cfg compileModule contents
  case result of
    Right (stmts,_) -> return stmts
    Left err -> throwError err
    where replace c r = map (\x -> if x == c then r else x)
compileImport i =
  error $ "Import syntax not supported. " ++
        "The compiler writer was too lazy to support that.\n" ++
        "It was: " ++ show i

-- | Compile Haskell declaration.
compileDecls :: Bool -> [Decl] -> Compile [JsStmt]
compileDecls toplevel decls = do
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
    -- Just ignore type aliases and signatures.
    TypeDecl{} -> return []
    TypeSig{} -> return []
    InfixDecl{} -> return []
    ClassDecl{} -> return []
    InstDecl{} -> return [] -- FIXME: Ignore.
    _ -> throwError (UnsupportedDeclaration decl)

-- | Compile a top-level pattern bind.
compilePatBind :: Bool -> Maybe Type -> Decl -> Compile [JsStmt]
compilePatBind toplevel sig pat = do
  case pat of
    PatBind _ (PVar ident) Nothing (UnGuardedRhs rhs) (BDecls []) ->
      case ffiExp rhs of
        Just formatstr -> case sig of
          Just sig -> compileFFI ident formatstr sig
          Nothing  -> throwError (FfiNeedsTypeSig pat)
        _ -> compileNormalPatBind toplevel ident rhs
    PatBind _ (PVar ident) Nothing (UnGuardedRhs rhs) bdecls ->
      compileNormalPatBind toplevel ident (Let bdecls rhs)
    _ -> throwError (UnsupportedDeclaration pat)

  where ffiExp (App (Var (UnQual (Ident "ffi"))) (Lit (String formatstr))) = Just formatstr
        ffiExp _ = Nothing

-- | Compile an FFI call.
compileFFI :: Name   -- ^ Name of the to-be binding.
           -> String -- ^ The format string.
           -> Type   -- ^ Type signature.
           -> Compile [JsStmt]
compileFFI name formatstr sig = do
  inner <- formatFFI formatstr (zip params funcFundamentalTypes)
  case JS.parse (printJS (wrapReturn inner)) (prettyPrint name) of
    Left err -> throwError (FfiFormatInvalidJavaScript inner err)
    Right{}  -> fmap return (bindToplevel True (UnQual name) (body inner))

  where body inner = foldr wrapParam (wrapReturn inner) params
        wrapParam name inner = JsFun [name] [] (Just inner)
        params = zipWith const uniqueNames [1..typeArity sig]
        wrapReturn inner = thunk $
          case lastMay funcFundamentalTypes of
            -- Returns a “pure” value;
            Just{} -> unserialize returnType (JsRawExp inner)
            -- Base case:
            Nothing -> JsRawExp inner
        funcFundamentalTypes = functionTypeArgs sig
        returnType = last funcFundamentalTypes

-- | Format the FFI format string with the given arguments.
formatFFI :: String                      -- ^ The format string.
          -> [(JsParam,FundamentalType)] -- ^ Arguments.
          -> Compile String              -- ^ The JS code.
formatFFI formatstr args = go formatstr where
  go ('%':'*':xs) = do
    these <- mapM inject (zipWith const [1..] args)
    rest <- go xs
    return (intercalate "," these ++ rest)
  go ('%':'%':xs) = do
    rest <- go xs
    return ('%' : rest)
  go ['%'] = throwError FfiFormatIncompleteArg
  go ('%':(span isDigit -> (op,xs))) = do
    case readMay op of
     Nothing -> throwError (FfiFormatBadChars op)
     Just n -> do
       this <- inject n
       rest <- go xs
       return (this ++ rest)
  go (x:xs) = do rest <- go xs
                 return (x : rest)
  go [] = return []

  inject n =
    case listToMaybe (drop (n-1) args) of
      Nothing -> throwError (FfiFormatNoSuchArg n)
      Just (arg,typ) -> do
        return (printJS (serialize typ (JsName arg)))

-- | Serialize a value to native JS, if possible.
serialize :: FundamentalType -> JsExp -> JsExp
serialize typ exp =
  case typ of
--    UnknownType -> force exp
    _ -> JsApp (JsName (hjIdent "serialize"))
               [typeRep typ,exp]

-- | Get a JS-representation of a fundamental type for encoding/decoding.
typeRep :: FundamentalType -> JsExp
typeRep typ =
  case typ of
    FunctionType xs -> JsList [JsLit $ JsStr "function",JsList (map typeRep xs)]
    JsType x        -> JsList [JsLit $ JsStr "action",JsList [typeRep x]]
    ListType x      -> JsList [JsLit $ JsStr "list",JsList [typeRep x]]
    typ             -> JsList [JsLit $ JsStr nom]
      where nom = case typ of
              StringType -> "string"
              DoubleType -> "double"
              BoolType   -> "bool"
              DateType   -> "date"
              _          -> "unknown"

-- | Get arg types of a function type.
functionTypeArgs :: Type -> [FundamentalType]
functionTypeArgs t =
  case t of
    TyForall _ _ i -> functionTypeArgs i
    TyFun a b      -> argType a : functionTypeArgs b
    TyParen st     -> functionTypeArgs st
    r              -> [argType r]

-- | Convert a Haskell type to an internal FFI representation.
argType :: Type -> FundamentalType
argType t =
  case t of
    TyApp (TyCon "Fay") a -> JsType (argType a)
    TyCon "String"        -> StringType
    TyCon "Double"        -> DoubleType
    TyCon "Bool"          -> BoolType
    TyFun x xs            -> FunctionType (argType x : functionTypeArgs xs)
    TyList x              -> ListType (argType x)
    TyParen st            -> argType st
    _                     -> UnknownType

-- | Get the arity of a type.
typeArity :: Type -> Int
typeArity t =
  case t of
    TyForall _ _ i -> typeArity i
    TyFun _ b      -> 1 + typeArity b
    TyParen st     -> typeArity st
    _              -> 0

-- | Compile a normal simple pattern binding.
compileNormalPatBind :: Bool -> Name -> Exp -> Compile [JsStmt]
compileNormalPatBind toplevel ident rhs = do
  body <- compileExp rhs
  bind <- bindToplevel toplevel (UnQual ident) (thunk body)
  return [bind]

-- | Compile a data declaration.
compileDataDecl :: Bool -> Decl -> [QualConDecl] -> Compile [JsStmt]
compileDataDecl toplevel decl constructors = do
  fmap concat $
    forM constructors $ \(QualConDecl _ _ _ condecl) ->
      case condecl of
        ConDecl (UnQual -> name) types  -> fmap return (makeDataCons name types [])
        RecDecl (UnQual -> name) fields -> do
          cons <- makeDataCons name (map snd fields) (map fst fields)
          funs <- makeAccessors (zip [1..] (map fst fields))
          return (cons : funs)
        _ -> throwError (UnsupportedDeclaration decl)

  where makeDataCons name types fields = do
          let slots = (map (fromString . ("slot"++) . show . fst)
                           (zip [1 :: Integer ..] types))
          return $
            JsVar name
                  (foldr (\slot inner -> JsFun [slot] [] (Just inner))
                         (thunk (JsList ((JsNew (hjIdent "Constructor")
                                                (JsLit (JsStr (qname name)) :
                                                 concat (map (map (JsLit . JsStr . unname)) fields)))
                                         : map JsName slots)))
                         slots)
        makeAccessors fields = do
          fmap concat $
            forM fields $ \(i,field) ->
              forM field $ \name ->
                bindToplevel toplevel
                             (UnQual name)
                             (JsFun ["x"]
                                    []
                                    (Just (thunk (JsIndex i (force (JsName "x"))))))

-- | Extract the string from a qname.
qname :: QName -> String
qname (UnQual (Ident str)) = str
qname _ = error "qname: Expected unqualified ident." -- FIXME:

-- | Extra the string from an ident.
unname :: Name -> String
unname (Ident str) = str
unname _ = error "Expected ident from uname." -- FIXME:

-- | Compile a function which pattern matches (causing a case analysis).
compileFunCase :: Bool -> [Match] -> Compile [JsStmt]
compileFunCase _toplevel [] = return []
compileFunCase toplevel matches@(Match _ name argslen _ _ _:_) = do
  tco <- config configTCO
  pats <- fmap optimizePatConditions $ forM matches $ \match@(Match _ _ pats _ rhs wheres) -> do
    unless (noBinds wheres) $ do _ <- throwError (UnsupportedWhereInMatch match) -- TODO: Support `where'.
                                 return ()
    exp <- compileRhs rhs
    foldM (\inner (arg,pat) -> do
             compilePat (JsName arg) pat inner)
          [JsEarlyReturn exp]
          (zip args pats)
  bind <- bindToplevel toplevel
                       (UnQual name)
                       (foldr (\arg inner -> JsFun [arg] [] (Just inner))
                              (stmtsThunk (let stmts = (concat pats ++ basecase)
                                           in if tco
                                                 then optimizeTailCalls args name stmts
                                                 else stmts))
                              args)
  return [bind]
  where args = zipWith const uniqueNames argslen
        basecase = if any isWildCardMatch matches
                      then []
                      else [throw ("unhandled case in " ++ show name)
                                  (JsList (map JsName args))]
        isWildCardMatch (Match _ _ pats _ _ _) = all isWildCardPat pats
        noBinds (BDecls []) = True
        noBinds (IPBinds []) = True
        noBinds _ = False

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
expand (JsApp (JsName (UnQual (Ident "_"))) xs) = do
  fmap concat (mapM flatten xs)
expand _ = Nothing

-- | Format a JS file using "js-beautify", or return the JS as-is if
--   "js-beautify" is unavailable
prettyPrintFile :: String -> IO String
prettyPrintFile file =
  (readAllFromProcess "js-beautify" file)
  >>= (either
       (\_ -> (readFile file) >>= (\js -> return $ js ++ "\n"))
       return)

-- | Compile a right-hand-side expression.
compileRhs :: Rhs -> Compile JsExp
compileRhs (UnGuardedRhs exp) = compileExp exp
compileRhs (GuardedRhss rhss) = compileGuards rhss

-- | Compile guards
compileGuards :: [GuardedRhs] -> Compile JsExp
compileGuards [] = return . JsThrowExp . JsLit . JsStr $ "Non-exhaustive guards"
compileGuards ((GuardedRhs _ (Qualifier (Var (UnQual (Ident "otherwise"))):_) exp):_) = compileExp exp
compileGuards ((GuardedRhs _ (Qualifier guard:_) exp):rest) =
  JsTernaryIf <$> fmap force (compileExp guard)
              <*> compileExp exp
              <*> compileGuards rest
compileGuards rhss = throwError . UnsupportedRhs . GuardedRhss $ rhss

-- | Compile a pattern match binding.
compileFunMatch :: Bool -> Match -> Compile [JsStmt]
compileFunMatch toplevel match =
  case match of
    (Match _ name args Nothing (UnGuardedRhs rhs) _) -> do
      body <- compileExp rhs
      args <- mapM patToArg args
      bind <- bindToplevel toplevel
                           (UnQual name)
                           (foldr (\arg inner -> JsFun [arg] [] (Just inner))
                                  (thunk body)
                                  args)
      return [bind]
    match -> throwError (UnsupportedMatchSyntax match)

  where patToArg (PVar name) = return (UnQual name)
        patToArg _           = throwError (UnsupportedMatchSyntax match)

instance CompilesTo Decl [JsStmt] where compileTo = compileDecl False

-- | Compile Haskell expression.
compileExp :: Exp -> Compile JsExp
compileExp exp =
  case exp of
    Paren exp                     -> compileExp exp
    Var (UnQual (Ident "return")) -> return (JsName (hjIdent "return"))
    Var qname                     -> return (JsName qname)
    Lit lit                       -> compileLit lit
    App exp1 exp2                 -> compileApp exp1 exp2
    InfixApp exp1 op exp2         -> compileInfixApp exp1 op exp2
    Let (BDecls decls) exp        -> compileLet decls exp
    List []                       -> return JsNull
    List xs                       -> compileList xs
    Tuple xs                      -> compileList xs
    If cond conseq alt            -> compileIf cond conseq alt
    Case exp alts                 -> compileCase exp alts
    Con (UnQual (Ident "True"))   -> return (JsName "true")
    Con (UnQual (Ident "False"))  -> return (JsName "false")
    Con exp                       -> return (JsName exp)
    Do stmts                      -> compileDoBlock stmts
    Lambda _ pats exp             -> compileLambda pats exp
    EnumFrom i                    -> do e <- compileExp i
                                        return (JsApp (JsName "enumFrom") [e])
    EnumFromTo i i'               -> do f <- compileExp i
                                        t <- compileExp i'
                                        return (JsApp (JsApp (JsName "enumFromTo") [f])
                                                      [t])
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

-- | Compile an infix application, optimizing the JS cases.
compileInfixApp :: Exp -> QOp -> Exp -> Compile JsExp
compileInfixApp exp1 op exp2 = do
  config <- config id
  case getOp op of
    UnQual (Symbol symbol)
      | symbol `elem` words "* + - / < > || &&" -> do
          e1 <- compileExp exp1
          e2 <- compileExp exp2
          return (JsInfix symbol (forceInlinable config e1) (forceInlinable config e2))
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

-- | Compile case expressions.
compileCase :: Exp -> [Alt] -> Compile JsExp
compileCase exp alts = do
  exp <- compileExp exp
  pats <- fmap optimizePatConditions $ mapM (compilePatAlt (JsName (tmpName exp))) alts
  return $
    (JsApp (JsFun [tmpName exp]
                  (concat pats)
                  (if any isWildCardAlt alts
                      then Nothing
                      else Just (throwExp "unhandled case" (JsName (tmpName exp)))))
           [exp])

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
            LetStmt{} -> throwError LetUnsupported
            RecStmt{} -> throwError RecursiveDoUnsupported

        compileGenerator srcloc pat inner exp = do
          let body = (Lambda srcloc [pat] inner)
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
compilePat exp pat body = do
  case pat of
    PVar name       -> return ([JsVar (UnQual name) exp] ++ body)
    PApp cons pats  -> compilePApp cons pats exp body
    PLit literal    -> compilePLit exp literal body
    PParen pat      -> compilePat exp pat body
    PWildCard       -> return body
    pat@PInfixApp{} -> compileInfixPat exp pat body
    PList pats      -> compilePList pats body exp
    PTuple pats     -> compilePList pats body exp
    PAsPat name pat -> compilePAsPat exp name pat body
    pat             -> throwError (UnsupportedPattern pat)

-- | Compile a literal value from a pattern match.
compilePLit :: JsExp -> Literal -> [JsStmt] -> Compile [JsStmt]
compilePLit exp literal body = do
  lit <- compileLit literal
  return [JsIf (equalExps exp lit)
               body
               []]

compilePAsPat :: JsExp -> Name -> Pat -> [JsStmt] -> Compile [JsStmt]
compilePAsPat exp name pat body = do
  x <- compilePat exp pat body;
  return ([JsVar (UnQual name) exp] ++ x ++ body)

-- | Equality test for two expressions, with some optimizations.
equalExps :: JsExp -> JsExp -> JsExp
equalExps a b
  | isConstant a && isConstant b = JsEq a b
  | isConstant a = JsEq a (force b)
  | isConstant b = JsEq (force a) b
  | otherwise =
     JsApp (JsName (hjIdent "equal")) [a,b]

-- | Is a JS expression a literal (constant)?
isConstant :: JsExp -> Bool
isConstant JsLit{} = True
isConstant _       = False

-- | Compile a pattern application.
compilePApp :: QName -> [Pat] -> JsExp -> [JsStmt] -> Compile [JsStmt]
compilePApp cons pats exp body = do
  let forcedExp = force exp
  substmts <- foldM (\body (i,pat) -> compilePat (JsIndex i forcedExp) pat body)
                    body
                    (reverse (zip [1..] pats))
  let constructor = JsIndex 0 forcedExp
      compareConstructorNames
        -- Special-casing on the booleans.
        | cons == "True" = JsEq forcedExp (JsLit (JsBool True))
        | cons == "False" = JsEq forcedExp (JsLit (JsBool False))
        -- Everything else, generic:
        | otherwise =
            JsEq (JsGetProp constructor "name")
                 (JsLit (JsStr (qname cons)))
  return [JsIf compareConstructorNames
               substmts
               []]

-- | Compile a pattern list.
compilePList :: [Pat] -> [JsStmt] -> JsExp -> Compile [JsStmt]
compilePList [] body exp =
  return [JsIf (JsEq (force exp) JsNull) body []]
compilePList pats body exp = do
  let forcedExp = force exp
  substmts <- foldM (\body (i,pat) -> compilePat (JsApp (JsApp (JsName (hjIdent "index"))
                                                               [JsLit (JsInt i)])
                                                        [forcedExp])
                                                 pat body)
                    body
                    (reverse (zip [0..] pats))
  return substmts

-- | Compile an infix pattern (e.g. cons and tuples.)
compileInfixPat :: JsExp -> Pat -> [JsStmt] -> Compile [JsStmt]
compileInfixPat exp pat@(PInfixApp left (Special cons) right) body =
  case cons of
    Cons -> do
      let forcedExp = JsName (tmpName exp)
          x = (JsGetProp forcedExp "car")
          xs = (JsGetProp forcedExp "cdr")
      rightMatch <- compilePat xs right body
      leftMatch <- compilePat x left rightMatch
      return [JsVar (tmpName exp) (force exp)
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

--------------------------------------------------------------------------------
-- Compilation utilities

-- | Generate unique names.
uniqueNames :: [JsParam]
uniqueNames = map (fromString . ("$_" ++))
            $ map return "abcxyz" ++
              zipWith (:) (cycle "v")
                          (map show [1 :: Integer ..])

-- | Optimize pattern matching conditions by merging conditions in common.
optimizePatConditions :: [[JsStmt]] -> [[JsStmt]]
optimizePatConditions = concat . map merge . groupBy sameIf where
  sameIf [JsIf cond1 _ _] [JsIf cond2 _ _] = cond1 == cond2
  sameIf _ _ = False
  merge xs@([JsIf cond _ _]:_) =
    [[JsIf cond (concat (optimizePatConditions (map getIfConsequent xs))) []]]
  merge noifs = noifs
  getIfConsequent [JsIf _ cons _] = cons
  getIfConsequent other = other

-- | Throw a JS exception.
throw :: String -> JsExp -> JsStmt
throw msg exp = JsThrow (JsList [JsLit (JsStr msg),exp])

-- | Throw a JS exception (in an expression).
throwExp :: String -> JsExp -> JsExp
throwExp msg exp = JsThrowExp (JsList [JsLit (JsStr msg),exp])

-- | Is an alt a wildcard?
isWildCardAlt :: Alt -> Bool
isWildCardAlt (Alt _ pat _ _) = isWildCardPat pat

-- | Is a pattern a wildcard?
isWildCardPat :: Pat -> Bool
isWildCardPat PWildCard{} = True
isWildCardPat PVar{}      = True
isWildCardPat _           = False

-- | A temporary name for testing conditions and such.
tmpName :: JsExp -> JsName
tmpName exp =
  fromString $
    case exp of
      JsName (qname -> x) -> "$_" ++ x
      _ -> ":tmp"

-- | Wrap an expression in a thunk.
thunk :: JsExp -> JsExp
-- thunk exp = JsNew (hjIdent "Thunk") [JsFun [] [] (Just exp)]
thunk exp =
  case exp of
    -- JS constants don't need to be in thunks, they're already strict.
    JsLit{} -> exp
    JsName "true" -> exp
    JsName "false" -> exp
    -- Functions (e.g. lets) used for introducing a new lexical scope
    -- aren't necessary inside a thunk. This is a simple aesthetic
    -- optimization.
    JsApp fun@JsFun{} [] -> JsNew ":thunk" [fun]
    -- Otherwise make a regular thunk.
    _ -> JsNew ":thunk" [JsFun [] [] (Just exp)]

-- | Wrap an expression in a thunk.
monad :: JsExp -> JsExp
monad exp = JsNew (hjIdent "Monad") [exp]

-- | Wrap an expression in a thunk.
stmtsThunk :: [JsStmt] -> JsExp
stmtsThunk stmts = JsNew ":thunk" [JsFun [] stmts Nothing]

unserialize :: FundamentalType -> JsExp -> JsExp
unserialize typ exp =
  JsApp (JsName (hjIdent "unserialize"))
        [typeRep typ,exp]

-- | Force an expression in a thunk.
force :: JsExp -> JsExp
force exp
  | isConstant exp = exp
  | otherwise = JsApp (JsName "_") [exp]

-- | Force an expression in a thunk.
forceInlinable :: CompileConfig -> JsExp -> JsExp
forceInlinable config exp
  | isConstant exp = exp
  | configInlineForce config =
    JsParen (JsTernaryIf (exp `JsInstanceOf` ":thunk")
                         (JsApp (JsName "_") [exp])
                         exp)
  | otherwise = JsApp (JsName "_") [exp]

-- | Resolve operators to only built-in (for now) functions.
resolveOpToVar :: QOp -> Compile Exp
resolveOpToVar op =
  case getOp op of
    UnQual (Symbol symbol)
      | symbol == "*"   -> return (Var (hjIdent "mult"))
      | symbol == "+"   -> return (Var (hjIdent "add"))
      | symbol == "-"   -> return (Var (hjIdent "sub"))
      | symbol == "/"   -> return (Var (hjIdent "div"))
      | symbol == "=="  -> return (Var (hjIdent "eq"))
      | symbol == "/="  -> return (Var (hjIdent "neq"))
      | symbol == ">"   -> return (Var (hjIdent "gt"))
      | symbol == "<"   -> return (Var (hjIdent "lt"))
      | symbol == ">="  -> return (Var (hjIdent "gte"))
      | symbol == "<="  -> return (Var (hjIdent "lte"))
      | symbol == "&&"  -> return (Var (hjIdent "and"))
      | symbol == "||"  -> return (Var (hjIdent "or"))
      | symbol == ">>=" -> return (Var (hjIdent "bind"))
      | symbol == ">>"  -> return (Var (hjIdent "then"))
      | otherwise       -> return (Var (fromString symbol))
    Special Cons        -> return (Var (hjIdent "cons"))
    _                   -> throwError (UnsupportedOperator op)

  where getOp (QVarOp op) = op
        getOp (QConOp op) = op

-- | Make an identifier from the built-in HJ module.
hjIdent :: String -> QName
hjIdent = Qual (ModuleName "Fay") . Ident

-- | Make a top-level binding.
bindToplevel :: Bool -> QName -> JsExp -> Compile JsStmt
bindToplevel toplevel name exp = do
  exportAll <- gets stateExportAll
  when (toplevel && exportAll) $ emitExport (EVar name)
  return (JsVar name exp)

-- | Emit exported names.
emitExport :: ExportSpec -> Compile ()
emitExport spec =
  case spec of
    EVar (UnQual name) -> modify $ \s -> s { stateExports = name : stateExports s }
    EVar _             -> error "Emitted a qualifed export, not supported."
    _ -> throwError (UnsupportedExportSpec spec)

--------------------------------------------------------------------------------
-- Utilities

-- | Parse result.
parseResult :: ((SrcLoc,String) -> b) -> (a -> b) -> ParseResult a -> b
parseResult fail ok result =
  case result of
    ParseOk a -> ok a
    ParseFailed srcloc msg -> fail (srcloc,msg)

-- | Get a config option.
config :: (CompileConfig -> a) -> Compile a
config f = gets (f . stateConfig)
