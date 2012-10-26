{-# LANGUAGE TemplateHaskell #-}

module Test.Api (tests) where

import Language.Fay

import Data.Default
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit                     (Assertion, assertBool)
import Test.Util

tests :: Test
tests = $testGroupGenerator

case_imports :: Assertion
case_imports = do
  res <- compileFile def { configTypecheck = False, configDirectoryIncludes = ["tests"] } "tests/RecordImport_Import.hs"
  assertBool "Could not compile file with imports" (isRight res)
