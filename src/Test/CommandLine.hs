{-# LANGUAGE TemplateHaskell #-}

module Test.CommandLine (tests) where

import           Control.Applicative
import           Data.Maybe
import           System.Process.Extra
import           Test.HUnit (Assertion, assertBool)
import           Test.Util
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH

tests :: Test
tests = $testGroupGenerator

compileFile :: [String] -> IO (Either String String)
compileFile flags = do
  fay <- fromJust <$> fayPath
  readAllFromProcess' fay flags ""

case_executable :: Assertion
case_executable = do
  fay <- fayPath
  assertBool "Could not find fay executable" (isJust fay)

case_compile :: Assertion
case_compile = do
  res <- compileFile ["--autorun", "--include=tests", "tests/RecordImport_Import.hs"]
  assertBool (fromLeft res) (isRight res)
