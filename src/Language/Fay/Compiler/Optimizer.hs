{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}

module Language.Fay.Compiler.Optimizer where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.Error
import Control.Monad.Writer
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
  in (newstmts ++ (tco (catMaybes (map (uncurryBinding newstmts) (nub uncurried)))))
  where st = OptState stmts []

-- | Perform any top-level cross-module optimizations and GO DEEP to
-- optimize further.
optimizeToplevel :: [JsStmt] -> Optimize [JsStmt]
optimizeToplevel = stripAndUncurry

-- | Perform tail-call optimization.
tco :: [JsStmt] -> [JsStmt]
tco = map inStmt where
  inStmt stmt =
    case stmt of
      JsMappedVar srcloc name exp -> JsMappedVar srcloc name (inject name exp)
      JsVar name exp -> JsVar name (inject name exp)
      e -> e
  inject name exp =
    case exp of
      JsFun params [] (Just (JsNew JsThunk [JsFun [] stmts ret])) ->
        JsFun params
              []
              (Just
                (JsNew JsThunk
                       [JsFun []
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
    swap stmt =
      case stmt of
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
  go frst args app = case app of
    JsApp (JsName JsForce) [e] -> if frst
                                     then do result <- go False args e
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
      stmts = [JsMappedVar (SrcLoc {srcFilename = "", srcLine = 1, srcColumn = 1}) (JsNameVar (Qual (ModuleName "Main") (Ident "sum$uncurried"))) (JsFun [JsParam 1,JsParam 2] [] (Just (JsNew JsThunk [JsFun [] [JsVar (JsNameVar (UnQual (Ident "acc"))) (JsName (JsParam 2)),JsIf (JsEq (JsApp (JsName JsForce) [JsName (JsParam 1)]) (JsLit (JsInt 0))) [JsEarlyReturn (JsName (JsNameVar (UnQual (Ident "acc"))))] [],JsVar (JsNameVar (UnQual (Ident "acc"))) (JsName (JsParam 2)),JsVar (JsNameVar (UnQual (Ident "n"))) (JsName (JsParam 1)),JsEarlyReturn (JsApp (JsName (JsNameVar (Qual (ModuleName "Main") (Ident "sum$uncurried")))) [JsApp (JsName (JsNameVar (Qual (ModuleName "Fay$") (Ident "sub$uncurried")))) [JsApp (JsName JsForce) [JsName (JsNameVar (UnQual (Ident "n")))],JsLit (JsInt 1)],JsApp (JsName (JsNameVar (Qual (ModuleName "Fay$") (Ident "add$uncurried")))) [JsApp (JsName JsForce) [JsName (JsNameVar (UnQual (Ident "acc")))],JsApp (JsName JsForce) [JsName (JsNameVar (UnQual (Ident "n")))]]])] Nothing])))]

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
