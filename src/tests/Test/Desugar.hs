{-# LANGUAGE TemplateHaskell #-}
module Test.Desugar
  ( tests
  , devTest
  , parseE
  , parseM
  , des
  ) where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           Language.Haskell.Exts.Annotated hiding (alt, binds, loc, name)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Groom

import           Fay.Compiler.Desugar            (desugar')
import           Fay.Compiler.Misc               (parseFay)
import           Fay.Types                       (CompileError (..))

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
  ]

parseAndDesugar :: String -> String -> IO (Module (), Either CompileError (Module ()))
parseAndDesugar name s =
  case parseFay "test" s :: ParseResult (Module SrcSpanInfo) of
    ParseFailed a b -> error $ show (name, a, b)
    ParseOk m' -> do
      let m = fmap (const ()) m'
      d <- desugar' False () m
      return (m,d)

doDesugar :: String -> String -> String -> Assertion
doDesugar testName a b = do
  (expected,Right e) <- parseAndDesugar (testName ++ " expected") a
  (_       ,Right t) <- parseAndDesugar (testName ++ " input"   ) b
  assertEqual "identity"  expected e
  assertEqual "desugared" expected t
  assertEqual "both"      t        e


-- When developing:

devTest :: String -> IO ()
devTest nam = do
  let (T _ a b) = fromJust (find (\(T n _ _) -> n == nam) testDeclarations)
  (originalExpected,Right desugaredExpected) <- parseAndDesugar "expected"   a
  (_               ,Right desugared        ) <- parseAndDesugar "test input" b
  if desugared == desugaredExpected && desugared == originalExpected
    then putStrLn "OK"
    else do
      putStrLn "--- desugared"
      g $ desugared
      putStrLn "--- desugaredExpected"
      g $ desugaredExpected
      putStrLn "--- originalExpected"
      g $ originalExpected
  when (desugared         /= desugaredExpected) $ putStrLn "desugared /= desugaredExpected"
  when (desugared         /= originalExpected ) $ putStrLn "desugared /= originalExpected"
  when (desugaredExpected /= originalExpected ) $ putStrLn "desugaredExpected /= undesugared"

g :: Show a => a -> IO ()
g = putStrLn . groom

parseM :: String -> Module ()
parseM s = let ParseOk m = parseFay "module" s :: ParseResult (Module SrcSpanInfo) in fmap (const ()) m

parseE :: String -> Exp ()
parseE s = let ParseOk m = parseFay "exp" s :: ParseResult (Exp SrcSpanInfo) in fmap (const ()) m

des :: String -> IO ()
des s = do
  Right r <- desugar' False () $ parseM s
  g r
