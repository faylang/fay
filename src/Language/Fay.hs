{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- | The Haskell→Javascript compiler.

module Language.Fay where

import           Language.Fay.Print              ()
import           Language.Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.IO
import           Data.List
import           Data.String
import           Language.Haskell.Exts

--------------------------------------------------------------------------------
-- Top level entry points

-- | Compile something that compiles to something else.
compile :: CompilesTo from to => from -> IO (Either CompileError to)
compile = runCompile . compileTo

-- | Run the compiler.
runCompile :: Compile a -> IO (Either CompileError a)
runCompile m = runErrorT m

-- | Compile a Haskell source string to a JavaScript source string.
compileViaStr :: (Show from,Show to,CompilesTo from to)
              => (from -> Compile to)
              -> String
              -> IO (Either CompileError String)
compileViaStr with from =
  runCompile (parseResult (throwError . uncurry ParseError)
                          (fmap printJS . with)
                          (parse from))

compileFromStr with from =
  parseResult (throwError . uncurry ParseError)
              (with)
              (parse from)

--------------------------------------------------------------------------------
-- Compilers

-- | Compile Haskell module.
compileModule :: Module -> Compile [JsStmt]
compileModule (Module _ modulename pragmas Nothing exports imports decls) = do
  imported <- fmap concat (mapM compileImport imports)
  current <- compileDecls decls
  return (imported ++ current)
compileModule mod = throwError (UnsupportedModuleSyntax mod)

instance CompilesTo Module [JsStmt] where compileTo = compileModule

-- | Compile the given import.
compileImport :: ImportDecl -> Compile [JsStmt]
compileImport (ImportDecl _ (ModuleName name) _ _ _ _ _)
  | isPrefixOf "Language.Fay." name || name == "Prelude" = return []
compileImport (ImportDecl _ (ModuleName name) False _ Nothing Nothing Nothing) = do
  contents <- io (readFile (replace '.' '/' name ++ ".hs"))
  compileFromStr compileModule contents
    where replace c r = map (\x -> if x == c then r else x)
compileImport i =
  error $ "Import syntax not supported. " ++
        "The compiler writer was too lazy to support that.\n" ++
        "It was: " ++ show i

-- | Compile Haskell declaration.
compileDecls :: [Decl] -> Compile [JsStmt]
compileDecls decls = do
  case decls of
    [] -> return []
    (TypeSig _ _ sig:bind@PatBind{}:decls) -> appendM (compilePatBind (Just sig) bind)
                                                      (compileDecls decls)
    (decl:decls) -> appendM (compileDecl decl)
                            (compileDecls decls)

  where appendM m n = do x <- m
                         xs <- n
                         return (x ++ xs)

compileDecl :: Decl -> Compile [JsStmt]
compileDecl decl =
  case decl of
    pat@PatBind{} -> compilePatBind Nothing pat
    FunBind matches -> compileFunCase matches
    DataDecl _ DataType _ _ _ constructors _ -> compileDataDecl decl constructors
    -- Just ignore type aliases and signatures.
    TypeDecl{} -> return []
    TypeSig{} -> return []
    InfixDecl{} -> return []
    ClassDecl{} -> return []
    InstDecl{} -> return [] -- FIXME: Ignore.
    _ -> throwError (UnsupportedDeclaration decl)

compilePatBind :: Maybe Type -> Decl -> ErrorT CompileError IO [JsStmt]
compilePatBind sig pat =
  case pat of
    PatBind _ (PVar ident) Nothing (UnGuardedRhs rhs) (BDecls []) ->
      case ffiExp rhs of
        Just detail@(binding,_,_) ->
          case sig of
            Nothing -> compileNormalPatBind ident rhs
            Just sig -> case () of
              () | func binding   -> compileFFIFunc sig ident detail
                 | method binding -> compileFFIMethod sig ident detail
                 | otherwise      -> throwError (FfiNeedsTypeSig pat)
        _ -> compileNormalPatBind ident rhs
    _ -> throwError (UnsupportedDeclaration pat)

  where func = flip elem ["foreignFay","foreignPure"]
        method = flip elem ["foreignMethodFay","foreignMethod"]
        ffiExp (App (App (Var (UnQual (Ident ident)))
                         (Lit (String name)))
                    (Lit (String typ)))
          = Just (ident,name,typ)
        ffiExp _ = Nothing

compileNormalPatBind :: Name -> Exp -> Compile [JsStmt]
compileNormalPatBind ident rhs = do
  body <- compileExp rhs
  return [JsVar (UnQual ident) (thunk body)]

compileFFIFunc :: Type -> Name -> (String,String,String) -> Compile [JsStmt]
compileFFIFunc sig ident detail@(_,name,_) = do
  let args = zipWith const uniqueNames [1..typeArity sig]
  compileFFI sig ident detail (JsRawName name) args args

compileFFIMethod :: Type -> Name -> (String,String,String) -> Compile [JsStmt]
compileFFIMethod sig ident detail@(_,name,_) = do
  let args = zipWith const uniqueNames [1..typeArity sig]
      jsargs = drop 1 args
      obj = head args
  compileFFI sig ident detail (JsGetProp (force (JsName obj)) (fromString name)) args jsargs

-- | Compile an FFI call.
compileFFI :: Type
           -> Name
           -> (String,String,String)
           -> JsExp
           -> [JsName]
           -> [JsName]
           -> Compile [JsStmt]
compileFFI sig ident (binding,_,typ) exp params args = do
  return [JsVar (UnQual ident)
                (foldr (\name inner -> JsFun [name] [] (Just inner))
                       (thunk
                        (maybeMonad
                         (unserialize typ
                                      (JsApp exp
                                             (map (\(typ,name) -> serialize typ (JsName name))
                                                  (zip types args))))))
                       params)]

  where (maybeMonad,types) | binding == "foreignFay"       = (monad,funcTypes)
                           | binding == "foreignMethodFay" = (monad,drop 1 funcTypes)
                           | binding == "foreignMethod"    = (id,drop 1 funcTypes)
                           | otherwise                     = (id,funcTypes)
        funcTypes = functionTypeArgs sig

-- | These are the data types that are serializable directly to native
-- JS data types. Strings, floating points and arrays. The others are:
-- actiosn in the JS monad, which are thunks that shouldn't be forced
-- when serialized but wrapped up as JS zero-arg functions, and
-- unknown types can't be converted but should at least be forced.
data ArgType = FunctionType | JsType | StringType | DoubleType | ListType | BoolType | UnknownType
  deriving (Show,Eq)

-- | Serialize a value to native JS, if possible.
serialize :: ArgType -> JsExp -> JsExp
serialize typ exp =
  JsApp (JsName (hjIdent "serialize"))
        [JsName (fromString (show typ)),exp]

-- | Get arg types of a function type.
functionTypeArgs :: Type -> [ArgType]
functionTypeArgs t =
  case t of
    TyForall _ _ i -> functionTypeArgs i
    TyFun a b      -> argType a : functionTypeArgs b
    TyParen st     -> functionTypeArgs st
    _              -> []

  where argType t =
          case t of
            TyApp (TyCon "Fay") _ -> JsType
            TyCon "String"       -> StringType
            TyCon "Double"       -> DoubleType
            TyCon "Bool"         -> BoolType
            TyFun{}              -> FunctionType
            TyList _             -> ListType
            _                    -> UnknownType

-- | Get the arity of a type.
typeArity :: Type -> Integer
typeArity t =
  case t of
    TyForall _ _ i -> typeArity i
    TyFun _ b      -> 1 + typeArity b
    TyParen st     -> typeArity st
    _              -> 0

compileDataDecl :: Decl -> [QualConDecl] -> Compile [JsStmt]
compileDataDecl decl constructors = do
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
                return (JsVar (UnQual name)
                              (JsFun ["x"]
                                     []
                                     (Just (thunk (JsIndex i (force (JsName "x")))))))

qname (UnQual (Ident str)) = str
qname _ = error "qname: Expected unqualified ident."

unname (Ident str) = str

-- | Compile a function which pattern matches (causing a case analysis).
compileFunCase :: [Match] -> Compile [JsStmt]
compileFunCase [] = return []
compileFunCase matches@(Match _ name argslen _ _ _:_) = do
  pats <- fmap optimizePatConditions $ forM matches $ \(Match _ _ pats _ rhs _) -> do
    exp <- compileRhs rhs
    foldM (\inner (arg,pat) -> do
             compilePat (JsName arg) pat inner)
          [JsEarlyReturn exp]
          (zip args pats)
  return [JsVar (UnQual name)
                (foldr (\arg inner -> JsFun [arg] [] (Just inner))
                       (stmtsThunk (concat pats ++ basecase))
                       args)]
  where args = zipWith const uniqueNames argslen
        basecase = if any isWildCardMatch matches
                      then []
                      else [throw ("unhandled case in " ++ show name)
                                  (JsList (map JsName args))]
        isWildCardMatch (Match _ _ pats _ _ _) = all isWildCardPat pats

-- | Compile a right-hand-side expression.
compileRhs :: Rhs -> Compile JsExp
compileRhs (UnGuardedRhs exp) = compileExp exp
compileRhs rhs                = throwError (UnsupportedRhs rhs)

-- | Compile a pattern match binding.
compileFunMatch :: Match -> Compile [JsStmt]
compileFunMatch match =
  case match of
    (Match _ name args Nothing (UnGuardedRhs rhs) _) -> do
      body <- compileExp rhs
      args <- mapM patToArg args
      return [JsVar (UnQual name)
                    (foldr (\arg inner -> JsFun [arg] [] (Just inner))
                           (thunk body)
                           args)]
    match -> throwError (UnsupportedMatchSyntax match)

  where patToArg (PVar name) = return (UnQual name)
        patToArg _           = throwError (UnsupportedMatchSyntax match)

instance CompilesTo Decl [JsStmt] where compileTo = compileDecl

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
                                        return (JsApp (JsName "enumFromTo") [f,t])
    ExpTypeSig _ e _ -> compileExp e

    exp -> throwError (UnsupportedExpression exp)

instance CompilesTo Exp JsExp where compileTo = compileExp

compileApp :: Exp -> Exp -> Compile JsExp
compileApp exp1 exp2 =
  JsApp <$> (forceFlatName <$> compileExp exp1)
        <*> fmap return (compileExp exp2)
  where forceFlatName name = JsApp (JsName "_") [name]

compileInfixApp :: Exp -> QOp -> Exp -> Compile JsExp
compileInfixApp exp1 op exp2 = do
  var <- resolveOpToVar op
  compileExp (App (App var exp1) exp2)

compileList :: [Exp] -> Compile JsExp
compileList xs = do
  exps <- mapM compileExp xs
  return (JsApp (JsName (hjIdent "list")) [JsList exps])

compileIf :: Exp -> Exp -> Exp -> Compile JsExp
compileIf cond conseq alt =
  JsTernaryIf <$> fmap force (compileExp cond)
              <*> compileExp conseq
              <*> compileExp alt

compileLambda :: [Pat] -> Exp -> Compile JsExp
compileLambda pats exp = do
  exp <- compileExp exp
  stmts <- foldM (\inner (param,pat) -> do
                   stmts <- compilePat (JsName param) pat inner
                   return [JsEarlyReturn (JsFun [param] (stmts ++ [unhandledcase param]) Nothing)])
                 [JsEarlyReturn exp]
                 (reverse (zip uniqueNames pats))
  return (JsApp (JsFun [] (stmts) Nothing) [])

  where unhandledcase = throw "unhandled case" . JsName

compileCase :: Exp -> [Alt] -> Compile JsExp
compileCase exp alts = do
  exp <- compileExp exp
  pats <- fmap optimizePatConditions $ mapM (compilePatAlt (JsName tmpName)) alts
  return $
    (JsApp (JsFun [tmpName]
                  (concat pats)
                  (if any isWildCardAlt alts
                      then Nothing
                      else Just (throwExp "unhandled case" (JsName tmpName))))
           [exp])

compileDoBlock :: [Stmt] -> Compile JsExp
compileDoBlock stmts = do
  doblock <- foldM compileStmt Nothing (reverse stmts)
  maybe (throwError EmptyDoBlock) compileExp doblock

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
    pat             -> throwError (UnsupportedPattern pat)

compilePLit :: JsExp -> Literal -> [JsStmt] -> Compile [JsStmt]
compilePLit exp literal body = do
  lit <- compileLit literal
  return [JsIf (JsApp (JsName (hjIdent "equal"))
                      [exp,lit])
               body
               []]

compilePApp :: QName -> [Pat] -> JsExp -> [JsStmt] -> Compile [JsStmt]
compilePApp cons pats exp body = do
  let forcedExp = force exp
  substmts <- foldM (\body (i,pat) -> compilePat (JsIndex i forcedExp) pat body)
                    body
                    (reverse (zip [1..] pats))
  return [JsIf (JsEq (JsGetProp (JsIndex 0 forcedExp) "name") (JsLit (JsStr (qname cons))))
               substmts
               []]

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

compileInfixPat :: JsExp -> Pat -> [JsStmt] -> Compile [JsStmt]
compileInfixPat exp pat@(PInfixApp left (Special cons) right) body =
  case cons of
    Cons -> do
      let forcedExp = JsName tmpName
          x = (JsGetProp forcedExp "car")
          xs = (JsGetProp forcedExp "cdr")
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
    decl@PatBind{} -> compileDecls [decl]
    decl@FunBind{} -> compileDecls [decl]
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

thenm :: JsExp -> JsExp -> JsExp
thenm e inner =
  JsApp (JsApp (JsName (hjIdent "then"))
               [e])
        [inner]

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
tmpName :: JsName
tmpName = ":tmp"

-- | Wrap an expression in a thunk.
thunk :: JsExp -> JsExp
thunk exp = JsNew (hjIdent "Thunk") [JsFun [] [] (Just exp)]

-- | Wrap an expression in a thunk.
monad :: JsExp -> JsExp
monad exp = JsNew (hjIdent "Monad") [exp]

-- | Wrap an expression in a thunk.
stmtsThunk :: [JsStmt] -> JsExp
stmtsThunk stmts = JsNew (hjIdent "Thunk") [JsFun [] stmts Nothing]

unserialize :: String -> JsExp -> JsExp
unserialize typ exp =
  JsApp (JsName (hjIdent "unserialize")) [JsLit (JsStr typ),exp]

-- | Force an expression in a thunk.
force :: JsExp -> JsExp
force exp =
  JsApp (JsName "_") [exp]

-- | Force an expression in a thunk.
monadValue :: JsExp -> JsExp
monadValue exp =
  JsGetProp (forceNoMemoize exp) "value"

-- | Force an expression in a thunk.
forceNoMemoize :: JsExp -> JsExp
forceNoMemoize exp =
  JsApp (JsName (hjIdent "force")) [exp,JsLit (JsBool True)]

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

--------------------------------------------------------------------------------
-- Utilities

-- | Parse result.
parseResult :: ((SrcLoc,String) -> b) -> (a -> b) -> ParseResult a -> b
parseResult fail ok result =
  case result of
    ParseOk a -> ok a
    ParseFailed srcloc msg -> fail (srcloc,msg)
