{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE TupleSections     #-}

-- | Optimizing the outputted JavaScript(-ish) AST.

module Fay.Compiler.Optimizer where

import           Fay.Compiler.Misc
import           Fay.Types

import           Control.Applicative
import           Control.Arrow                   (first)
import           Control.Monad.Error
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List
import           Data.Maybe
import qualified Fay.Exts.NoAnnotation           as N
import           Language.Haskell.Exts.Annotated hiding (app, name, op)

import           Prelude                         hiding (exp)

-- | The arity of a function. Arity here is defined to be the number
-- of arguments that can be directly uncurried from a curried lambda
-- abstraction. So \x y z -> if x then (\a -> a) else (\a -> a) has an
-- arity of 3, not 4.
type FuncArity = (N.QName,Int)

-- | Optimize monad.
type Optimize = State OptState

-- | State.
data OptState = OptState
  { optStmts   :: [JsStmt]
  , optUncurry :: [N.QName]
  }

-- | Run an optimizer, which may output additional statements.
runOptimizer :: ([JsStmt] -> Optimize [JsStmt]) -> [JsStmt] -> [JsStmt]
runOptimizer optimizer stmts =
  let (newstmts,OptState _ uncurried) = flip runState st $ optimizer stmts
  in newstmts ++ (tco . mapMaybe (uncurryBinding newstmts) $ nub uncurried)
  where st = OptState stmts []

-- | Inline x >> y to x;y in the JS output.
inlineMonad :: [JsStmt] -> [JsStmt]
inlineMonad = map go where
  go stmt = case stmt of
    JsVar name exp          -> JsVar name (inline exp)
    JsIf exp stmts stmts'   -> JsIf (inline exp) (map go stmts) (map go stmts')
    JsEarlyReturn exp       -> JsEarlyReturn (inline exp)
    JsThrow exp             -> JsThrow (inline exp)
    JsWhile exp stmts       -> JsWhile (inline exp) (map go stmts)
    JsUpdate name exp       -> JsUpdate name (inline exp)
    JsSetProp a b exp       -> JsSetProp a b (inline exp)
    JsSetQName s a exp      -> JsSetQName s a (inline exp)
    JsSetModule a exp       -> JsSetModule a (inline exp)
    JsSetConstructor a exp  -> JsSetConstructor a (inline exp)
    JsSetPropExtern a b exp -> JsSetPropExtern a b (inline exp)
    JsContinue              -> JsContinue
    JsBlock stmts           -> JsBlock (map go stmts)
    JsExpStmt exp           -> JsExpStmt (inline exp)

  inline expr = case expr of
    -- Optimizations
    JsApp op args -> fromMaybe (JsApp (inline op) $ map inline args) (flatten expr)

    -- Plumbing
    JsFun nm names stmts mexp        -> JsFun nm names (map go stmts) (fmap inline mexp)

    JsNegApp exp                     -> JsNegApp (inline exp)
    JsTernaryIf exp1 exp2 exp3       -> JsTernaryIf (inline exp1) (inline exp2) (inline exp3)
    JsParen exp                      -> JsParen (inline exp)
    JsGetProp exp name               -> JsGetProp (inline exp) name
    JsLookup exp exp2                -> JsLookup (inline exp) (inline exp2)
    JsUpdateProp exp name exp2       -> JsUpdateProp (inline exp) name (inline exp2)
    JsGetPropExtern exp string       -> JsGetPropExtern (inline exp) string
    JsUpdatePropExtern exp name exp2 -> JsUpdatePropExtern (inline exp) name (inline exp2)
    JsList exps                      -> JsList (map inline exps)
    JsNew name exps                  -> JsNew name (map inline exps)
    JsThrowExp exp                   -> JsThrowExp (inline exp)
    JsInstanceOf exp name            -> JsInstanceOf (inline exp) name
    JsIndex i exp                    -> JsIndex i (inline exp)
    JsEq exp exp2                    -> JsEq (inline exp) (inline exp2)
    JsNeq exp exp2                   -> JsNeq (inline exp) (inline exp2)
    JsInfix string exp exp2          -> JsInfix string (inline exp) (inline exp2)
    JsObj keyvals                    -> JsObj keyvals
    rest                             -> rest

-- | Flatten a a>>(b>>c) to [a,b,c].
flatten :: JsExp -> Maybe JsExp
flatten exp = case collect exp of
  Just (stmts@(_:_:_)) -> let s = reverse stmts
                          in Just $ thunk (JsSeq (map force (init s) ++ [last s]))
  _ -> Nothing

-- | Try to collect nested a>>(b>>c).
collect :: JsExp -> Maybe [JsExp]
collect exp = case exp of
  JsApp op args | isThen op ->
    case args of
      [rest,x] -> (x :) <$> collect rest
      [x]  -> return [x]
      _ -> Nothing
  _ -> return [exp]

  where
    isThen (JsName (JsNameVar (Qual _ (ModuleName _ m) (Ident _ n)))) = m == "Fay$" && n == "then$uncurried"
    isThen _ = False


-- | Perform any top-level cross-module optimizations and GO DEEP to
-- optimize further.
optimizeToplevel :: [JsStmt] -> Optimize [JsStmt]
optimizeToplevel = stripAndUncurry

-- | Perform tail-call optimization.
tco :: [JsStmt] -> [JsStmt]
tco = map inStmt where
  inStmt stmt = case stmt of
    JsVar name exp -> JsVar name (inject name exp)
    e -> e
  inject name exp = case exp of
    JsFun nm params [] (Just (JsNew JsThunk [JsFun _ [] stmts ret])) ->
      JsFun nm params
            []
            (Just
              (JsNew JsThunk
                     [JsFun Nothing []
                            (optimize params name (stmts ++ [ JsEarlyReturn e | Just e <- [ret] ]))
                            Nothing]))
    _ -> exp
  optimize params name stmts = result where
    result = let (newstmts,w) = runWriter makeWhile
             in if null w
                   then stmts
                   else newstmts
    makeWhile = do
      newstmts <- fmap concat (mapM swap stmts)
      return [JsWhile (JsLit (JsBool True)) newstmts]
    swap stmt = case stmt of
      JsEarlyReturn e
        | tailCall e -> do tell [()]
                           return (rebind e ++ [JsContinue])
        | otherwise  -> return [stmt]
      JsIf p ithen ielse -> do
        newithen <- fmap concat (mapM swap ithen)
        newielse <- fmap concat (mapM swap ielse)
        return [JsIf p newithen newielse]
      e -> return [e]
    tailCall (JsApp (JsName cname) _) = cname == name
    tailCall _ = False
    rebind (JsApp _ args) = zipWith go args params where
      go arg param = JsUpdate param arg
    rebind e = error . show $ e

-- | Strip redundant forcing from the whole generated code.
stripAndUncurry :: [JsStmt] -> Optimize [JsStmt]
stripAndUncurry = applyToExpsInStmts stripFuncForces where
  stripFuncForces arities exp = case exp of
    JsApp (JsName JsForce) [JsName (JsNameVar f)]
      | Just _ <- lookup f arities -> return (JsName (JsNameVar f))
    JsFun nm ps stmts body         -> do substmts <- mapM stripInStmt stmts
                                         sbody <- maybe (return Nothing) (fmap Just . go) body
                                         return (JsFun nm ps substmts sbody)
    JsApp a b                      -> do
      result <- walkAndStripForces arities exp
      case result of
        Just strippedExp           -> go strippedExp
        Nothing                    -> JsApp <$> go a <*> mapM go b
    JsNegApp e                     -> JsNegApp <$> go e
    JsTernaryIf a b c              -> JsTernaryIf <$> go a <*> go b <*> go c
    JsParen e                      -> JsParen <$> go e
    JsUpdateProp e n a             -> JsUpdateProp <$> go e <*> pure n <*> go a
    JsList xs                      -> JsList <$> mapM go xs
    JsEq a b                       -> JsEq <$> go a <*> go b
    JsInfix op a b                 -> JsInfix op <$> go a <*> go b
    JsObj xs                       -> JsObj <$> mapM (\(x,y) -> (x,) <$> go y) xs
    JsNew name xs                  -> JsNew name <$> mapM go xs
    e                              -> return e

    where
      go = stripFuncForces arities
      stripInStmt = applyToExpsInStmt arities stripFuncForces

-- | Strip redundant forcing from an application if possible.
walkAndStripForces :: [FuncArity] -> JsExp -> Optimize (Maybe JsExp)
walkAndStripForces arities = go True [] where
  go frst args app = case app of
    JsApp (JsName JsForce) [e] ->
      if frst
        then do
          result <- go False args e
          case result of
            Nothing -> return Nothing
            Just ex -> return (Just (JsApp (JsName JsForce) [ex]))
        else go False args e
    JsApp op [arg] -> go False (arg:args) op
    JsName (JsNameVar f)
      | Just arity <- lookup f arities, length args == arity -> do
        modify $ \s -> s { optUncurry = f : optUncurry s }
        return (Just (JsApp (JsName (JsNameVar (renameUncurried f))) args))
    _ -> return Nothing

-- | Apply the given function to the top-level expressions in the
-- given statements.
applyToExpsInStmts :: ([FuncArity] -> JsExp -> Optimize JsExp) -> [JsStmt] -> Optimize [JsStmt]
applyToExpsInStmts f stmts = mapM (applyToExpsInStmt (collectFuncs stmts) f) stmts

-- | Apply the given function to the top-level expressions in the
-- given statement.
applyToExpsInStmt :: [FuncArity] -> ([FuncArity] -> JsExp -> Optimize JsExp) -> JsStmt -> Optimize JsStmt
applyToExpsInStmt funcs f stmts = uncurryInStmt stmts where
  transform = f funcs
  uncurryInStmt stmt = case stmt of
    JsVar name exp              -> JsVar name <$> transform exp
    JsEarlyReturn exp           -> JsEarlyReturn <$> transform exp
    JsIf op ithen ielse         -> JsIf <$> transform op
                                        <*> mapM uncurryInStmt ithen
                                        <*> mapM uncurryInStmt ielse
    s -> pure s

-- | Collect functions and their arity from the whole codeset.
collectFuncs :: [JsStmt] -> [FuncArity]
collectFuncs = (++ prim) . concatMap collectFunc where
  collectFunc (JsVar (JsNameVar name) exp) | arity > 0 = [(name,arity)]
    where arity = expArity exp
  collectFunc _ = []
  prim = map (first (Qual () (ModuleName () "Fay$"))) (unary ++ binary)
  unary = map (,1) [Ident () "return"]
  binary = map ((,2) . Ident ())
               ["then","bind","mult","mult","add","sub","div"
               ,"eq","neq","gt","lt","gte","lte","and","or"]

-- | Get the arity of an expression.
expArity :: JsExp -> Int
expArity (JsFun _ _ _ mexp) = 1 + maybe 0 expArity mexp
expArity _ = 0

-- | Change foo(x)(y) to foo$uncurried(x,y).
uncurryBinding :: [JsStmt] -> N.QName -> Maybe JsStmt
uncurryBinding stmts qname = listToMaybe (mapMaybe funBinding stmts)
  where
    funBinding stmt = case stmt of
      JsVar (JsNameVar name) body
        | name == qname -> JsVar (JsNameVar (renameUncurried name)) <$> uncurryIt body
      _ -> Nothing

    uncurryIt = Just . go [] where
      go args exp = case exp of
        JsFun _ [arg] [] (Just body) -> go (arg : args) body
        inner -> JsFun Nothing (reverse args) [] (Just inner)

-- | Rename an uncurried copy of a curried function.
renameUncurried :: N.QName -> N.QName
renameUncurried q = case q of
  Qual _ m n -> Qual () m (renameUnQual n)
  UnQual _ n -> UnQual () (renameUnQual n)
  s -> s
  where
    renameUnQual n = case n of
      Ident _ nom -> Ident () (nom ++ postfix)
      Symbol _ nom -> Symbol () (nom ++ postfix)
    postfix = "$uncurried"
