{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Compile (tests) where

import           Fay
import           Fay.Compiler.Config
import           Fay.System.Process.Extra

import           Control.Applicative
import           Control.Monad
import           Data.Default
import           Data.Maybe
import           Language.Haskell.Exts.Annotated
import           System.Environment
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit                      (Assertion, assertBool,
                                                  assertEqual, assertFailure)
import           Test.Util

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
    Right (_,r) -> assertBool "RecordImport_Export was not added to stateImported" .
                     isJust . lookup (ModuleName () "RecordImport_Export") $ stateImported r

fp :: FilePath
fp = "tests/RecordImport_Import.hs"

case_stateRecordTypes :: Assertion
case_stateRecordTypes = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFileWithState defConf { configPackageConf = whatAGreatFramework } "tests/Compile/Records.hs"
  case res of
    Left err -> error (show err)
    Right (_,r) ->
      -- TODO order should not matter
      assertEqual "stateRecordTypes mismatch"
        [ (UnQual () (Ident () "T"),[UnQual () (Symbol () ":+")])
        , (UnQual () (Ident () "R"),[UnQual () (Ident () "R"), UnQual () (Ident () "S")])
        ]
        (stateRecordTypes r)

case_importStateRecordTypes :: Assertion
case_importStateRecordTypes = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFileWithState defConf { configPackageConf = whatAGreatFramework } "tests/Compile/ImportRecords.hs"
  case res of
    Left err -> error (show err)
    Right (_,r) ->
      -- TODO order should not matter
      assertEqual "stateRecordTypes mismatch"
        [ (UnQual () (Ident () "T"),[UnQual () (Symbol () ":+")])
        , (UnQual () (Ident () "R"),[UnQual () (Ident () "R"), UnQual () (Ident () "S")])
        ]
        (stateRecordTypes r)

case_typecheckCPP :: Assertion
case_typecheckCPP = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFile defConf { configPackageConf = whatAGreatFramework, configTypecheck = True, configFilePath = Just "tests/Compile/CPPTypecheck.hs" } "tests/Compile/CPPTypecheck.hs"
  either (assertFailure . show) (const $ return ()) res

case_cppMultiLineStrings :: Assertion
case_cppMultiLineStrings = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFile defConf { configPackageConf = whatAGreatFramework, configTypecheck = True, configFilePath = Just "tests/Compile/CPPMultiLineStrings.hs" } "tests/Compile/CPPMultiLineStrings.hs"
  either (assertFailure . show) (const $ return ()) res

case_strictWrapper :: Assertion
case_strictWrapper = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFile defConf { configPackageConf = whatAGreatFramework, configTypecheck = True, configFilePath = Just "tests/Compile/StrictWrapper.hs", configStrict = ["StrictWrapper"] } "tests/Compile/StrictWrapper.hs"
  (\a b -> either a b res) (assertFailure . show) $ \js -> do
    writeFile "tests/Compile/StrictWrapper.js" js
    (err, out) <- either id id <$> readAllFromProcess "node" ["tests/Compile/StrictWrapper.js"] ""
    when (err /= "") $ assertFailure err
    assertEqual "strictWrapper node stdout" "3\n" out

defConf :: CompileConfig
defConf = addConfigDirectoryIncludePaths ["tests/"]
        $ def { configTypecheck = False }
