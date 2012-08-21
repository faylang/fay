module Test.Api (tests) where

import           Data.Default
import           Language.Fay.Compiler
import           Language.Fay.Types
import           Test.HUnit
import           Test.Util

tests :: Test
tests = TestList [TestCase $ do
  res <- compileFile def { configDirectoryIncludes = ["tests"] } "tests/RecordImport_Import.hs"
  assertBool "Could not compile file with imports" (isRight res)]
