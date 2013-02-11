{-# OPTIONS -fno-warn-name-shadowing -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Compile expressions

module Fay.Compiler.Exp where

import Fay.Compiler.Misc
import Fay.Compiler.Pattern
import Fay.Compiler.Print
import Fay.Types

import Control.Applicative
import Control.Monad.Error
import Control.Monad.RWS
import Data.Maybe
import Language.Haskell.Exts

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

-- | Compiling instance.
instance CompilesTo Exp JsExp where compileTo = compileExp

-- | Compile variable.
compileVar :: QName -> Compile JsExp
compileVar qname = do
  qname <- resolveName qname
  return (JsName (JsNameVar qname))

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

-- | Compile simple application.
compileApp :: Exp -> Exp -> Compile JsExp
compileApp exp1 exp2 = do
   flattenApps <- config configFlattenApps
   jsexp1 <- compileExp exp1
   case jsexp1 of
     JsName (JsNameVar qname) -> do
       ntc <- lookupNewtypeConst qname
       ntd <- lookupNewtypeDest  qname
       if isJust ntc || isJust ntd
         then compileExp exp2
         else (if flattenApps then method2 else method1) jsexp1
     _ -> (if flattenApps then method2 else method1) jsexp1
   where
  -- Method 1:
  -- In this approach code ends up looking like this:
  -- a(a(a(a(a(a(a(a(a(a(L)(c))(b))(0))(0))(y))(t))(a(a(F)(3*a(a(d)+a(a(f)/20))))*a(a(f)/2)))(140+a(f)))(y))(t)})
  -- Which might be OK for speed, but increases the JS stack a fair bit.
  method1 exp1 =
    JsApp <$> (forceFlatName <$> return exp1)
          <*> fmap return (compileExp exp2)
  forceFlatName name = JsApp (JsName JsForce) [name]

  -- Method 2:
  -- In this approach code ends up looking like this:
  -- d(O,a,b,0,0,B,w,e(d(I,3*e(e(c)+e(e(g)/20))))*e(e(g)/2),140+e(g),B,w)}),d(K,g,e(c)+0.05))
  -- Which should be much better for the stack and readability, but probably not great for speed.
  method2 exp1 = fmap flatten $
    JsApp <$> return exp1
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
compileInfixApp exp1 ap exp2 = compileExp (App (App (Var op) exp1) exp2)

  where op = getOp ap
        getOp (QVarOp op) = op
        getOp (QConOp op) = op

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
  compileDecls <- asks readerCompileDecls
  v <- case decl of
    decl@PatBind{} -> compileDecls False [decl]
    decl@FunBind{} -> compileDecls False [decl]
    TypeSig{}      -> return []
    _              -> throwError (UnsupportedLetBinding decl)
  return v

-- | Compile a list expression.
compileList :: [Exp] -> Compile JsExp
compileList xs = do
  exps <- mapM compileExp xs
  return (makeList exps)

-- | Compile an if.
compileIf :: Exp -> Exp -> Exp -> Compile JsExp
compileIf cond conseq alt =
  JsTernaryIf <$> fmap force (compileExp cond)
              <*> compileExp conseq
              <*> compileExp alt

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

-- | Compile the given pattern against the given expression.
compilePatAlt :: JsExp -> Alt -> Compile [JsStmt]
compilePatAlt exp alt@(Alt _ pat rhs wheres) = case wheres of
  BDecls (_ : _) -> throwError (UnsupportedWhereInAlt alt)
  IPBinds (_ : _) -> throwError (UnsupportedWhereInAlt alt)
  _ -> withScope $ do
    generateScope $ compilePat exp pat []
    alt <- compileGuardedAlt rhs
    compilePat exp pat [alt]

-- | Compile a guarded alt.
compileGuardedAlt :: GuardedAlts -> Compile JsStmt
compileGuardedAlt alt =
  case alt of
    UnGuardedAlt exp -> JsEarlyReturn <$> compileExp exp
    GuardedAlts alts -> compileGuards (map altToRhs alts)
   where
    altToRhs (GuardedAlt l s e) = GuardedRhs l s e

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

-- | Compile a do block.
compileDoBlock :: [Stmt] -> Compile JsExp
compileDoBlock stmts = do
  doblock <- foldM compileStmt Nothing (reverse stmts)
  maybe (throwError EmptyDoBlock) compileExp doblock

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

-- | Desugar left sections to lambdas.
desugarLeftSection :: Exp -> QOp -> Compile Exp
desugarLeftSection e o = withScopedTmpName $ \tmp ->
    return (Lambda undefined [PVar tmp] (InfixApp e o (Var (UnQual tmp))))

-- | Desugar left sections to lambdas.
desugarRightSection :: QOp -> Exp -> Compile Exp
desugarRightSection o e = withScopedTmpName $ \tmp ->
    return (Lambda undefined [PVar tmp] (InfixApp (Var (UnQual tmp)) o e))

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

-- | Update a record (using clever Object.create(), dunno if this is cross-browser).
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

-- | Make a Fay list.
makeList :: [JsExp] -> JsExp
makeList exps = (JsApp (JsName (JsBuiltIn "list")) [JsList exps])

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
