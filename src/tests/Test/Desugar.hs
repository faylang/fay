module Test.Desugar
  ( tests
  , devTest
  , parseE
  , parseM
  , des
  ) where

import           Fay.Compiler.Prelude

import           Fay.Compiler.Desugar
import           Fay.Compiler.Parse              (parseFay)
import           Fay.Types.CompileError          (CompileError (..))

import           Language.Haskell.Exts.Annotated hiding (alt, binds, loc, name)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Groom

tests :: TestTree
tests = testGroup "desugar" $ map (\(T k a b) -> testCase k $ doDesugar k a b) testDeclarations

data DesugarTest = T String String String

testDeclarations :: [DesugarTest]
testDeclarations =
  [T "LambdaCase"
     "import Prelude; f = \\gen0 -> case gen0 of { _ -> 2 }"
     "import Prelude; f = \\case { _ -> 2 }"
  ,T "MultiWayIf"
     "import Prelude; f = case () of { _ | True -> 1 | False -> 2 }"
     "import Prelude; f = if | True -> 1 | False -> 2"
  ,T "TupleCon"
     "import Prelude; f = \\gen0 gen1 gen2 -> (gen0, gen1, gen2)"
     "import Prelude; f = (,,)"
  ,T "Do"
     "import Prelude; f = (>>=) x (\\gen0 -> (>>) y z)"
     "import Prelude; f = do { gen0 <- x; y; z }"
  ,T "TupleSection"
     "import Prelude; f = \\gen0 gen1 -> (gen0,2,gen1)"
     "import Prelude; f = (,2,)"
  ,T "ImplicitPrelude1" -- Add missing Prelude import
     "import Prelude"
     ""
  ,T "ImplicitPrelude2" -- Keep existing Prelude import
     "import Prelude ()"
     "import Prelude ()"
  ,T "ImplicitPrelude3" -- Keep existing qualified import
     "import qualified Prelude"
     "import qualified Prelude"
  ,T "NoImplicitPrelude"
     "{-# LANGUAGE NoImplicitPrelude #-}"
     "{-# LANGUAGE NoImplicitPrelude #-}"
  ,T "OperatorSectionRight"
     "import Prelude; f = \\gen0 -> g gen0 x"
     "import Prelude; f = (`g` x)"
  ,T "OperatorSectionLeft"
     "import Prelude; f = \\gen0 -> g x gen0"
     "import Prelude; f = (x `g`)"
  ,T "InfixOpOp"
     "import Prelude; f = (+) x y"
     "import Prelude; f = x + y"
  ,T "InfixOpFun"
     "import Prelude; f = g x y"
     "import Prelude; f = x `g` y"
  ,T "InfixOpCons"
     "import Prelude; f = (:) x y"
     "import Prelude; f = x : y"
  ,T "ExpParen"
     "import Prelude; f = x y"
     "import Prelude; f = (x (y))"
  ,T "PatParen"
     "import Prelude; f x = y"
     "import Prelude; f (x) = y"
  ,T "PatInfixOp"
     "import Prelude; f ((:) x y) = z"
     "import Prelude; f (x : y) = z"
  ,T "PatFieldPun"
     "import Prelude; f R { x = x } = y"
     "import Prelude; f R { x } = y"
  ,T "PatField"
     "import Prelude; f = R { x = x }"
     "import Prelude; f = R { x }"
  ,T "FFITopLevel"
     "import Prelude; f :: Int; f = ffi \"1\" :: Int"
     "import Prelude; f :: Int; f = ffi \"1\""
  ,T "FFIWhere"
     "import Prelude; f = () where x :: Int; x = ffi \"1\" :: Int"
     "import Prelude; f = () where x :: Int; x = ffi \"1\""
  ]

parseAndDesugar :: String -> String -> IO (Module SrcLoc, Either CompileError (Module SrcLoc))
parseAndDesugar name s =
  case parseFay "test" s :: ParseResult (Module SrcLoc) of
    ParseFailed a b -> error $ show (name, a, b)
    ParseOk m -> do
      d <- desugar' "gen" noLoc m
      return (m,d)

doDesugar :: String -> String -> String -> Assertion
doDesugar testName a b = do
  (originalExpected, desugaredExpected, desugared) <- parseAndDesugarAll testName a b
  assertEqual "identity"  (unAnn originalExpected) (unAnn desugaredExpected)
  assertEqual "desugared" (unAnn originalExpected) (unAnn desugared        )
  assertEqual "both"      (unAnn desugared       ) (unAnn desugaredExpected)

parseAndDesugarAll :: String -> String -> String -> IO (Module SrcLoc, Module SrcLoc, Module SrcLoc)
parseAndDesugarAll testName a b = do
  (originalExpected',Right desugaredExpected) <- parseAndDesugar (testName ++ " expected")   a
  (_                ,Right desugared        ) <- parseAndDesugar (testName ++ " input") b
  -- We need to desugar parens in the original module since we
  -- strip it away in desugaring but there isn't alawys a way to construct
  -- this paren directly from a source string
  let originalExpected = desugarPatParen . desugarExpParen $ originalExpected'
  return (originalExpected, desugaredExpected, desugared)

-- When developing:

devTest :: String -> IO ()
devTest nam = do
  let (T _ a b) = fromJust (find (\(T n _ _) -> n == nam) testDeclarations)
  (originalExpected, desugaredExpected, desugared) <- parseAndDesugarAll "" a b
  if unAnn desugared == unAnn desugaredExpected && unAnn desugared == unAnn originalExpected
    then putStrLn "OK"
    else do
      putStrLn "--- originalExpected"
      g $ unAnn originalExpected
      putStrLn "--- desugaredExpected"
      g $ unAnn desugaredExpected
      putStrLn "--- desugared"
      g $ unAnn desugared
      putStrLn "--- originalExpected"
      pretty originalExpected
      putStrLn "--- desugaredExpected"
      pretty desugaredExpected
      putStrLn "--- desugared"
      pretty desugared
  when (unAnn originalExpected  /= unAnn desugaredExpected) $ putStrLn "originalExpected /= desugaredExpected"
  when (unAnn desugared         /= unAnn desugaredExpected) $ putStrLn "desugared /= desugaredExpected"
  when (unAnn desugared         /= unAnn originalExpected ) $ putStrLn "desugared /= originalExpected"
  when (unAnn desugaredExpected /= unAnn originalExpected ) $ putStrLn "desugaredExpected /= undesugared"

g :: Show a => a -> IO ()
g = putStrLn . groom

unAnn :: Functor f => f a -> f ()
unAnn = void

pretty :: Module SrcLoc -> IO ()
pretty = putStrLn . prettyPrint

parseM :: String -> Module ()
parseM s = let ParseOk m = parseFay "module" s :: ParseResult (Module SrcSpanInfo) in unAnn m

parseE :: String -> Exp ()
parseE s = let ParseOk m = parseFay "exp" s :: ParseResult (Exp SrcSpanInfo) in unAnn m

des :: String -> IO ()
des s = do
  Right r <- desugar' "gen" () $ parseM s
  g r
