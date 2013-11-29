{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Compile expressions.

module Fay.Compiler.Exp
  (compileExp
  ,compileGuards
  ,compileLetDecl
  ,compileLit
  ) where

import           Fay.Compiler.FFI                (compileFFIExp)
import           Fay.Compiler.Misc
import           Fay.Compiler.Pattern
import           Fay.Compiler.Print
import           Fay.Compiler.QName
import           Fay.Data.List.Extra
import           Fay.Exts.NoAnnotation           (unAnn)
import           Fay.Exts.Scoped                 (noI)
import qualified Fay.Exts.Scoped                 as S
import           Fay.Types

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.RWS
import qualified Data.Char                       as Char
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Names

-- | Compile Haskell expression.
compileExp :: S.Exp -> Compile JsExp
compileExp exp = case exp of
  Paren _ exp                        -> compileExp exp
  Var _ qname                        -> compileVar qname
  Lit _ lit                          -> compileLit lit
  App _ exp1 exp2                    -> compileApp exp1 exp2
  NegApp _ exp                       -> compileNegApp exp
  InfixApp _ exp1 op exp2            -> compileInfixApp exp1 op exp2
  Let _ (BDecls _ decls) exp         -> compileLet decls exp
  List _ []                          -> return JsNull
  List _ xs                          -> compileList xs
  Tuple _ _boxed xs                  -> compileList xs
  If _ cond conseq alt               -> compileIf cond conseq alt
  Case _ exp alts                    -> compileCase exp alts
  Con _ (UnQual _ (Ident _ "True"))  -> return (JsLit (JsBool True))
  Con _ (UnQual _ (Ident _ "False")) -> return (JsLit (JsBool False))
  Con _ qname                        -> compileVar qname
  Lambda _ pats exp                  -> compileLambda pats exp
  EnumFrom _ i                       -> compileEnumFrom i
  EnumFromTo _ i i'                  -> compileEnumFromTo i i'
  EnumFromThen _ a b                 -> compileEnumFromThen a b
  EnumFromThenTo _ a b z             -> compileEnumFromThenTo a b z
  RecConstr _ name fieldUpdates      -> compileRecConstr name fieldUpdates
  RecUpdate _ rec  fieldUpdates      -> compileRecUpdate rec fieldUpdates
  ListComp {}                        -> shouldBeDesugared exp
  Do {}                              -> shouldBeDesugared exp
  LeftSection {}                     -> shouldBeDesugared exp
  RightSection {}                    -> shouldBeDesugared exp
  TupleSection {}                    -> shouldBeDesugared exp
  ExpTypeSig _ exp sig               ->
    case ffiExp exp of
      Nothing -> compileExp exp
      Just formatstr -> compileFFIExp (S.srcSpanInfo $ ann exp) Nothing formatstr sig

  exp -> throwError (UnsupportedExpression exp)

-- | Compile variable.
compileVar :: S.QName -> Compile JsExp
compileVar (Special _ t@TupleCon{}) = shouldBeDesugared t
compileVar qname = do
    nc <- lookupNewtypeConst qname
    nd <- lookupNewtypeDest qname
    if (nc /= Nothing || nd /= Nothing)
      then -- variable is either a newtype constructor or newtype destructor,
           -- replace it with identity function
           return idFun
      else do
        qname <- unsafeResolveName qname
        return (JsName (JsNameVar qname))
  where
    idFun = JsFun Nothing [JsTmp 1] [] (Just (JsName $ JsTmp 1))

-- | Compile Haskell literal.
compileLit :: S.Literal -> Compile JsExp
compileLit lit = case lit of
  Char _ ch _      -> return (JsLit (JsChar ch))
  Int _ integer _   -> return (JsLit (JsInt (fromIntegral integer))) -- FIXME:
  Frac _ rational _ -> return (JsLit (JsFloating (fromRational rational)))
  String _ string _ -> do
    fromString <- gets stateUseFromString
    if fromString
      then return (JsLit (JsStr string))
      else return (JsApp (JsName (JsBuiltIn "list")) [JsLit (JsStr string)])
  lit           -> throwError (UnsupportedLiteral lit)

-- | Compile simple application.
compileApp :: S.Exp -> S.Exp -> Compile JsExp
compileApp exp1@(Con _ q) exp2 =
  maybe (compileApp' exp1 exp2) (const $ compileExp exp2) =<< lookupNewtypeConst q
compileApp exp1@(Var _ q) exp2 =
  maybe (compileApp' exp1 exp2) (const $ compileExp exp2) =<< lookupNewtypeDest q
compileApp exp1 exp2 =
  compileApp' exp1 exp2

-- | Helper for compileApp.
compileApp' :: S.Exp -> S.Exp -> Compile JsExp
compileApp' exp1 exp2 = do
  flattenApps <- config configFlattenApps
  jsexp1 <- compileExp exp1
  (if flattenApps then method2 else method1) jsexp1 exp2
    where
    -- Method 1:
    -- In this approach code ends up looking like this:
    -- a(a(a(a(a(a(a(a(a(a(L)(c))(b))(0))(0))(y))(t))(a(a(F)(3*a(a(d)+a(a(f)/20))))*a(a(f)/2)))(140+a(f)))(y))(t)})
    -- Which might be OK for speed, but increases the JS stack a fair bit.
    method1 :: JsExp -> S.Exp -> Compile JsExp
    method1 exp1 exp2 =
      JsApp <$> (forceFlatName <$> return exp1)
            <*> fmap return (compileExp exp2)
      where
        forceFlatName name = JsApp (JsName JsForce) [name]

    -- Method 2:
    -- In this approach code ends up looking like this:
    -- d(O,a,b,0,0,B,w,e(d(I,3*e(e(c)+e(e(g)/20))))*e(e(g)/2),140+e(g),B,w)}),d(K,g,e(c)+0.05))
    -- Which should be much better for the stack and readability, but probably not great for speed.
    method2 :: JsExp -> S.Exp -> Compile JsExp
    method2 exp1 exp2 = fmap flatten $
      JsApp <$> return exp1
            <*> fmap return (compileExp exp2)
      where
        flatten (JsApp op args) =
         case op of
           JsApp l r -> JsApp l (r ++ args)
           _        -> JsApp (JsName JsApply) (op : args)
        flatten x = x

-- | Compile a negate application
compileNegApp :: S.Exp -> Compile JsExp
compileNegApp e = JsNegApp . force <$> compileExp e

-- | Compile an infix application, optimizing the JS cases.
compileInfixApp :: S.Exp -> S.QOp -> S.Exp -> Compile JsExp
compileInfixApp exp1 ap exp2 = case exp1 of
  Con _ q -> do
    newtypeConst <- lookupNewtypeConst q
    case newtypeConst of
      Just _ -> compileExp exp2
      Nothing -> normalApp
  _ -> normalApp
  where
    normalApp = compileExp (App noI (App noI (Var noI op) exp1) exp2)
    op = getOp ap
    getOp (QVarOp _ op) = op
    getOp (QConOp _ op) = op

-- | Compile a let expression.
compileLet :: [S.Decl] -> S.Exp -> Compile JsExp
compileLet decls exp = do
  binds <- mapM compileLetDecl decls
  body <- compileExp exp
  return (JsApp (JsFun Nothing [] [] (Just $ stmtsThunk $ concat binds ++ [JsEarlyReturn body])) [])

-- | Compile let declaration.
compileLetDecl :: S.Decl -> Compile [JsStmt]
compileLetDecl decl = do
  compileDecls <- asks readerCompileDecls
  case decl of
    decl@PatBind{} -> compileDecls False [decl]
    decl@FunBind{} -> compileDecls False [decl]
    TypeSig{}      -> return []
    _              -> throwError (UnsupportedLetBinding decl)

-- | Compile a list expression.
compileList :: [S.Exp] -> Compile JsExp
compileList xs = do
  exps <- mapM compileExp xs
  return (makeList exps)

-- | Compile an if.
compileIf :: S.Exp -> S.Exp -> S.Exp -> Compile JsExp
compileIf cond conseq alt =
  JsTernaryIf <$> fmap force (compileExp cond)
              <*> compileExp conseq
              <*> compileExp alt

-- | Compile case expressions.
compileCase :: S.Exp -> [S.Alt] -> Compile JsExp
compileCase exp alts = do
  exp <- compileExp exp
  withScopedTmpJsName $ \tmpName -> do
    pats <- fmap optimizePatConditions $ mapM (compilePatAlt (JsName tmpName)) alts
    return $
      JsApp (JsFun Nothing
                   [tmpName]
                   (concat pats)
                   (if any isWildCardAlt alts
                       then Nothing
                       else Just (throwExp "unhandled case" (JsName tmpName))))
            [exp]

-- | Compile the given pattern against the given expression.
compilePatAlt :: JsExp -> S.Alt -> Compile [JsStmt]
compilePatAlt exp alt@(Alt _ pat rhs wheres) = case wheres of
  Just (BDecls _ (_ : _)) -> throwError (UnsupportedWhereInAlt alt)
  Just (IPBinds _ (_ : _)) -> throwError (UnsupportedWhereInAlt alt)
  _ -> do
    alt <- compileGuardedAlt rhs
    compilePat exp pat [alt]

-- | Compile a guarded alt.
compileGuardedAlt :: S.GuardedAlts -> Compile JsStmt
compileGuardedAlt alt =
  case alt of
    UnGuardedAlt _ exp -> JsEarlyReturn <$> compileExp exp
    GuardedAlts _ alts -> compileGuards (map altToRhs alts)
   where
    altToRhs (GuardedAlt l s e) = GuardedRhs l s e

-- | Compile guards
compileGuards :: [S.GuardedRhs] -> Compile JsStmt
compileGuards ((GuardedRhs _ (Qualifier _ (Var _ (UnQual _ (Ident _ "otherwise"))):_) exp):_) =
  (\e -> JsIf (JsLit $ JsBool True) [JsEarlyReturn e] []) <$> compileExp exp
compileGuards (GuardedRhs _ (Qualifier _ guard:_) exp : rest) =
  makeIf <$> fmap force (compileExp guard)
         <*> compileExp exp
         <*> if null rest then return [] else do
           gs' <- compileGuards rest
           return [gs']
    where makeIf gs e gss = JsIf gs [JsEarlyReturn e] gss

compileGuards rhss = throwError . UnsupportedRhs . GuardedRhss noI $ rhss

-- | Compile a lambda.
compileLambda :: [S.Pat] -> S.Exp -> Compile JsExp
compileLambda pats exp = do
  exp   <- compileExp exp
  stmts <- generateStatements exp
  case stmts of
    [JsEarlyReturn fun@JsFun{}] -> return fun
    _ -> error "Unexpected statements in compileLambda"

  where unhandledcase = throw "unhandled case" . JsName
        allfree = all isWildCardPat pats
        generateStatements exp =
          foldM (\inner (param,pat) -> do
                  stmts <- compilePat (JsName param) pat inner
                  return [JsEarlyReturn (JsFun Nothing [param] (stmts ++ [unhandledcase param | not allfree]) Nothing)])
                [JsEarlyReturn exp]
                (reverse (zip uniqueNames pats))

-- | Compile [e1..] arithmetic sequences.
compileEnumFrom :: S.Exp -> Compile JsExp
compileEnumFrom i = do
  e <- compileExp i
  return (JsApp (JsName (JsNameVar (Qual () "Prelude" "enumFrom"))) [e])

-- | Compile [e1..e3] arithmetic sequences.
compileEnumFromTo :: S.Exp -> S.Exp -> Compile JsExp
compileEnumFromTo i i' = do
  f <- compileExp i
  t <- compileExp i'
  cfg <- config id
  return $ case optEnumFromTo cfg f t of
    Just s -> s
    _ -> JsApp (JsApp (JsName (JsNameVar (Qual () "Prelude" "enumFromTo"))) [f]) [t]

-- | Compile [e1,e2..] arithmetic sequences.
compileEnumFromThen :: S.Exp -> S.Exp -> Compile JsExp
compileEnumFromThen a b = do
  fr <- compileExp a
  th <- compileExp b
  return (JsApp (JsApp (JsName (JsNameVar (Qual () "Prelude" "enumFromThen"))) [fr]) [th])

-- | Compile [e1,e2..e3] arithmetic sequences.
compileEnumFromThenTo :: S.Exp -> S.Exp -> S.Exp -> Compile JsExp
compileEnumFromThenTo a b z = do
  fr <- compileExp a
  th <- compileExp b
  to <- compileExp z
  cfg <- config id
  return $ case optEnumFromThenTo cfg fr th to of
    Just s -> s
    _ -> JsApp (JsApp (JsApp (JsName (JsNameVar (Qual () "Prelude" "enumFromThenTo"))) [fr]) [th]) [to]

-- | Compile a record construction with named fields
-- | GHC will warn on uninitialized fields, they will be undefined in JS.
compileRecConstr :: S.QName -> [S.FieldUpdate] -> Compile JsExp
compileRecConstr name fieldUpdates = do
  -- var obj = new $_Type()
  let unQualName = withIdent lowerFirst . unQualify $ unAnn name
  qname <- unsafeResolveName name
  let record = JsVar (JsNameVar unQualName) (JsNew (JsConstructor qname) [])
  setFields <- liftM concat (forM fieldUpdates (updateStmt name))
  return $ JsApp (JsFun Nothing [] (record:setFields) (Just . JsName . JsNameVar . withIdent lowerFirst . unQualify $ unAnn name)) []
  where
    -- updateStmt :: QName a -> S.FieldUpdate -> Compile [JsStmt]
    updateStmt (unAnn -> o) (FieldUpdate _ (unAnn -> field) value) = do
      exp <- compileExp value
      return [JsSetProp (JsNameVar $ withIdent lowerFirst $ unQualify o) (JsNameVar $ unQualify field) exp]
    updateStmt o (FieldWildcard (wildcardFields -> fields)) = do
      return $ for fields $ \fieldName -> JsSetProp (JsNameVar . withIdent lowerFirst . unQualify . unAnn $ o)
                                                    (JsNameVar fieldName)
                                                    (JsName $ JsNameVar fieldName)
    -- I couldn't find a code that generates (FieldUpdate (FieldPun ..))
    updateStmt _ u = error ("updateStmt: " ++ show u)

    wildcardFields l = case l of
      Scoped (RecExpWildcard es) _ -> map (unQualify . origName2QName) . map fst $ es
      _ -> []
    lowerFirst :: String -> String
    lowerFirst "" = ""
    lowerFirst (x:xs) = '_' : Char.toLower x : xs

-- | Compile a record update.
compileRecUpdate :: S.Exp -> [S.FieldUpdate] -> Compile JsExp
compileRecUpdate rec fieldUpdates = do
  record <- force <$> compileExp rec
  let copyName = UnQual () $ Ident () "$_record_to_update"
      copy = JsVar (JsNameVar copyName)
                   (JsRawExp ("Object.create(" ++ printJSString record ++ ")"))
  setFields <- forM fieldUpdates (updateExp copyName)
  return $ JsApp (JsFun Nothing [] (copy:setFields) (Just $ JsName $ JsNameVar copyName)) []
  where
    updateExp :: QName a -> S.FieldUpdate -> Compile JsStmt
    updateExp (unAnn -> copyName) (FieldUpdate _ (unQualify . unAnn -> field) value) =
      JsSetProp (JsNameVar copyName) (JsNameVar field) <$> compileExp value
    updateExp _ f@FieldPun{} = shouldBeDesugared f
    -- I also couldn't find a code that generates (FieldUpdate FieldWildCard)
    updateExp _ FieldWildcard{} = error "unsupported update: FieldWildcard"

-- | Make a Fay list.
makeList :: [JsExp] -> JsExp
makeList exps = JsApp (JsName $ JsBuiltIn "list") [JsList exps]

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

-- | Maximum number of elements to allow in strict list representation
-- of arithmetic sequences.
maxStrictASLen :: Int
maxStrictASLen = 10
