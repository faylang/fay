{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Api (tests) where

import Language.Fay

import Data.Default
import Data.Maybe
import Language.Haskell.Exts.Syntax
import System.Environment
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit                     (Assertion, assertBool, assertEqual, assertFailure)
import Test.Util

tests :: Test
tests = $testGroupGenerator

case_imports :: Assertion
case_imports = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFile defConf { configPackageConf = whatAGreatFramework } fp
  assertBool "Could not compile file with imports" (isRight res)

case_importedList :: Assertion
case_importedList = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFileWithState defConf { configPackageConf = whatAGreatFramework } fp
  case res of
    Left err -> error (show err)
    Right (_,r) -> assertBool ("RecordImport_Export was not added to stateImported" ++ show (stateImported r)) .
                     isJust . lookup (ModuleName "RecordImport_Export") $ stateImported r

case_stateRecordTypes :: Assertion
case_stateRecordTypes = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFileWithState defConf { configPackageConf = whatAGreatFramework } "tests/Api/Records.hs"
  case res of
    Left err -> error (show err)
    Right (_,r) -> do
      -- TODO order should not matter
      assertEqual "stateRecordTypes mismatch"
        [ (UnQual (Ident "T"),[UnQual (Symbol ":+")])
        , (UnQual (Ident "R"),[UnQual (Ident "R"), UnQual (Ident "S")])
        ]
        (stateRecordTypes r)

case_importStateRecordTypes :: Assertion
case_importStateRecordTypes = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFileWithState defConf { configPackageConf = whatAGreatFramework } "tests/Api/ImportRecords.hs"
  case res of
    Left err -> error (show err)
    Right (_,r) -> do
      -- TODO order should not matter
      assertEqual "stateRecordTypes mismatch"
        [ (UnQual (Ident "T"),[UnQual (Symbol ":+")])
        , (UnQual (Ident "R"),[UnQual (Ident "R"), UnQual (Ident "S")])
        ]
        (stateRecordTypes r)

case_typecheckCPP :: Assertion
case_typecheckCPP = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFile defConf { configPackageConf = whatAGreatFramework, configTypecheck = True, configFilePath = Just "tests/Api/CPPTypecheck.hs" } "tests/Api/CPPTypecheck.hs"
  either (assertFailure . show) (const $ return ()) res

fp :: FilePath
fp = "tests/RecordImport_Import.hs"

defConf :: CompileConfig
defConf = addConfigDirectoryIncludePaths ["tests/"]
        $ def { configTypecheck = False }
