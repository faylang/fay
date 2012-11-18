{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}

module Language.Fay.Compiler.Optimizer where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.State
import Data.List
import Data.Maybe
import Language.Fay.Print
import Language.Fay.Types
import Language.Haskell.Exts (QName(..),ModuleName(..),Name(..))
import Language.Haskell.Exts (SrcLoc(..))
import Prelude hiding (exp)

-- | The arity of a function. Arity here is defined to be the number
-- of arguments that can be directly uncurried from a curried lambda
-- abstraction. So \x y z -> if x then (\a -> a) else (\a -> a) has an
-- arity of 3, not 4.
type FuncArity = (QName,Int)

-- | Optimize monad.
type Optimize = State OptState

-- | State.
data OptState = OptState
  { optStmts   :: [JsStmt]
  , optUncurry :: [QName]
  }

-- | Run an optimizer, which may output additional statements.
runOptimizer :: ([JsStmt] -> Optimize [JsStmt]) -> [JsStmt] -> [JsStmt]
runOptimizer optimizer stmts =
  let (newstmts,OptState _ uncurried) = flip runState st $ optimizer stmts
  in (newstmts ++ (catMaybes (map (uncurryBinding newstmts) uncurried)))
  where st = OptState stmts []

-- | Perform any top-level cross-module optimizations and GO DEEP to
-- optimize further.
optimizeToplevel :: [JsStmt] -> Optimize [JsStmt]
optimizeToplevel = stripAndUncurry

-- | Strip redundant forcing from the whole generated code.
stripAndUncurry :: [JsStmt] -> Optimize [JsStmt]
stripAndUncurry = applyToExpsInStmts stripFuncForces where
  stripFuncForces arities exp =
    case exp of
      JsApp (JsName JsForce) [JsName (JsNameVar f)]
        | Just _ <- lookup f arities -> return (JsName (JsNameVar f))
      JsFun ps stmts body            -> do substmts <- mapM stripInStmt stmts
                                           sbody <- maybe (return Nothing) (fmap Just . go) body
                                           return (JsFun ps substmts sbody)
      JsApp a b                      -> do
        result <- walkAndStripForces arities exp
        case result of
          Just strippedExp             -> go strippedExp
          Nothing                      -> JsApp <$> go a <*> mapM go b
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

      where go = stripFuncForces arities
            stripInStmt = applyToExpsInStmt arities stripFuncForces

-- | Strip redundant forcing from an application if possible.
walkAndStripForces :: [FuncArity] -> JsExp -> Optimize (Maybe JsExp)
walkAndStripForces arities = go True [] where
  go first args app = case app of
    JsApp (JsName JsForce) [e] -> if first
                                     then do result <- go False args e
                                             case result of
                                               Nothing -> return Nothing
                                               Just e -> return (Just (JsApp (JsName JsForce) [e]))
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
  uncurryInStmt stmt =
    case stmt of
      JsMappedVar srcloc name exp -> JsMappedVar srcloc name <$> transform exp
      JsVar name exp              -> JsVar name <$> transform exp
      JsEarlyReturn exp           -> JsEarlyReturn <$> transform exp
      JsIf op ithen ielse         -> JsIf <$> transform op
                                          <*> mapM uncurryInStmt ithen
                                          <*> mapM uncurryInStmt ielse
      s -> pure s

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

test :: IO ()
test = do
  let (newstmts,OptState _ uncurried) = flip runState st $ optimizeToplevel stmts
  putStrLn $ printJSPretty newstmts
  putStrLn $ printJSPretty (catMaybes (map (uncurryBinding newstmts) uncurried))

    where
      st = OptState stmts []
      stmts = [JsMappedVar (SrcLoc {srcFilename = "<interactive>", srcLine = 1, srcColumn = 1}) (JsNameVar (Qual (ModuleName "Main") (Ident "f"))) (JsFun [JsParam 1] [] (Just (JsFun [JsParam 2] [] (Just (JsFun [JsParam 3] [] (Just (JsNew JsThunk [JsFun [] [JsVar (JsNameVar (UnQual (Ident "z"))) (JsName (JsParam 3)),JsVar (JsNameVar (UnQual (Ident "y"))) (JsName (JsParam 2)),JsVar (JsNameVar (UnQual (Ident "x"))) (JsName (JsParam 1)),JsEarlyReturn (JsLit (JsInt 1))] Nothing]))))))),JsMappedVar (SrcLoc {srcFilename = "<interactive>", srcLine = 1, srcColumn = 14}) (JsNameVar (Qual (ModuleName "Main") (Ident "a"))) (JsNew JsThunk [JsFun [] [] (Just (JsApp (JsApp (JsName JsForce) [JsApp (JsApp (JsName JsForce) [JsApp (JsApp (JsName JsForce) [JsName (JsNameVar (Qual (ModuleName "Main") (Ident "f")))]) [JsLit (JsInt 1)]]) [JsLit (JsInt 2)]]) [JsLit (JsInt 3)]))]),JsMappedVar (SrcLoc {srcFilename = "<interactive>", srcLine = 1, srcColumn = 27}) (JsNameVar (Qual (ModuleName "Main") (Ident "b"))) (JsNew JsThunk [JsFun [] [] (Just (JsApp (JsApp (JsName JsForce) [JsApp (JsApp (JsName JsForce) [JsName (JsNameVar (Qual (ModuleName "Main") (Ident "map")))]) [JsApp (JsApp (JsName JsForce) [JsApp (JsApp (JsName JsForce) [JsName (JsNameVar (Qual (ModuleName "Main") (Ident "f")))]) [JsLit (JsInt 1)]]) [JsLit (JsInt 2)]]]) [JsNull]))])]

uncurryBinding :: [JsStmt] -> QName -> Maybe JsStmt
uncurryBinding stmts qname = listToMaybe (mapMaybe funBinding stmts)

  where funBinding stmt =
          case stmt of
            JsMappedVar srcloc (JsNameVar name) body
              | name == qname -> JsMappedVar srcloc (JsNameVar (renameUncurried name)) <$> uncurryIt body
            JsVar (JsNameVar name) body
              | name == qname -> JsVar (JsNameVar (renameUncurried name)) <$> uncurryIt body
            _ -> Nothing

        uncurryIt = Just . go [] where
          go args exp =
              case exp of
                JsFun [arg] [] (Just body) -> go (arg : args) body
                inner -> JsFun (reverse args) [] (Just inner)

-- | Rename an uncurried copy of a curried function.
renameUncurried :: QName -> QName
renameUncurried q =
          case q of
            Qual m n -> Qual m (renameUnQual n)
            UnQual n -> UnQual (renameUnQual n)
            s -> s
  where renameUnQual n =
          case n of
            Ident nom -> Ident (nom ++ postfix)
            Symbol nom -> Symbol (nom ++ postfix)
        postfix = "$uncurried"
