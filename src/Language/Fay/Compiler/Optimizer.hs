{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}

module Language.Fay.Compiler.Optimizer where

import Control.Arrow
import Language.Fay.Types
import Language.Haskell.Exts (QName(..),ModuleName(..),Name(..))
import Prelude hiding (exp)

-- | The arity of a function. Arity here is defined to be the number
-- of arguments that can be directly uncurried from a curried lambda
-- abstraction. So \x y z -> if x then (\a -> a) else (\a -> a) has an
-- arity of 3, not 4.
type FuncArity = (QName,Int)

-- | Perform any top-level cross-module optimizations and GO DEEP to
-- optimize further.
optimizeToplevel :: [JsStmt] -> [JsStmt]
optimizeToplevel = stripRedundantForcing

-- | Strip redundant forcing from the whole generated code.
stripRedundantForcing :: [JsStmt] -> [JsStmt]
stripRedundantForcing stmts = map (stripRedunantForcesFromDecl funcs) stmts where
  funcs = collectFuncs stmts

-- | Strip out redundant forces from appropriate declaration types.
stripRedunantForcesFromDecl :: [FuncArity] -> JsStmt -> JsStmt
stripRedunantForcesFromDecl funcs = go where
  go stmt =
    case stmt of
      JsMappedVar srcloc name exp -> JsMappedVar srcloc name (strip exp)
      JsVar name exp              -> JsVar name (strip exp)
      JsEarlyReturn exp           -> JsEarlyReturn (strip exp)
      s                           -> s
  strip = stripFuncForces funcs

-- | Strip out forcing of functions that don't need to be
-- forced. E.g. _(f)(x) → f(x).
stripFuncForces :: [FuncArity] -> JsExp -> JsExp
stripFuncForces arities exp =
  case exp of
    JsApp (JsName JsForce) [JsName (JsNameVar f)]
      | Just _ <- lookup f arities -> JsName (JsNameVar f)
    JsFun ps stmts body            -> JsFun ps (map (stripRedunantForcesFromDecl arities) stmts) (fmap go body)
    JsApp a b                      -> JsApp (go a) (map go b)
    JsNegApp e                     -> JsNegApp (go e)
    JsTernaryIf a b c              -> JsTernaryIf (go a) (go b) (go c)
    JsParen e                      -> JsParen (go e)
    JsUpdateProp e n a             -> JsUpdateProp (go e) n (go a)
    JsList xs                      -> JsList (map go xs)
    JsEq a b                       -> JsEq (go a) (go b)
    JsInfix op a b                 -> JsInfix op (go a) (go b)
    JsObj xs                       -> JsObj (map (second go) xs)
    JsNew name xs                  -> JsNew name (map go xs)
    e                              -> e

  where go = stripFuncForces arities

-- | Collect functions and their arity from the whole codeset.
collectFuncs :: [JsStmt] -> [FuncArity]
collectFuncs = (++ prim) . concat . map collectFunc where
  collectFunc (JsMappedVar _ name exp) = collectFunc (JsVar name exp)
  collectFunc (JsVar (JsNameVar name) exp) | arity > 0 = [(name,arity)]
    where arity = expArity exp
  collectFunc _ = []
  prim = map (first (Qual (ModuleName "Fay$"))) (unary ++ binary)
  unary = map (,1) [Ident "return"]
  binary = map ((,2) . Ident)
               ["then","bind","mult","mult","add","sub","div"
               ,"eq","neq","gt","lt","gte","lte","and","or"]

-- | Get the arity of an expression.
expArity :: JsExp -> Int
expArity (JsFun _ _ mexp) = 1 + maybe 0 expArity mexp
expArity _ = 0
