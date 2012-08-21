module Test.CommandLine (tests) where

import           Control.Applicative
import           Data.Maybe
import           System.Process.Extra
import           Test.HUnit
import           Test.Util

compileFile :: [String] -> IO (Either String String)
compileFile flags = do
  fay <- fromJust <$> fayPath
  readAllFromProcess' fay flags ""

tests :: Test
tests = TestList [
    TestCase $ do
      fay <- fayPath
      assertBool "Could not find fay executable" (isJust fay)
  , TestCase $ do
      res <- compileFile ["--autorun", "--include=tests", "tests/RecordImport_Import.hs"]
      assertBool (fromLeft res) (isRight res)
  ]
