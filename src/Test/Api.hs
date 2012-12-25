{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Api (tests) where

import           Language.Fay

import           Data.Default
import           Data.Maybe
import           Language.Haskell.Exts.Syntax
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit                     (Assertion, assertBool, assertEqual)
import           Test.Util

tests :: Test
tests = $testGroupGenerator

case_imports :: Assertion
case_imports = do
  res <- compileFile defConf fp
  assertBool "Could not compile file with imports" (isRight res)

case_importedList :: Assertion
case_importedList = do
  res <- compileFileWithState defConf fp
  case res of
    Left err -> error (show err)
    Right (_,r) -> assertBool "RecordImport_Export was not added to stateImported" .
                     isJust . lookup (ModuleName "RecordImport_Export") $ stateImported r

case_stateRecordTypes :: Assertion
case_stateRecordTypes = do
  res <- compileFileWithState defConf "tests/Api/Records.hs"
  case res of
    Left err -> error (show err)
    Right (_,r) -> do
      -- TODO order should not matter
      assertEqual "stateRecordTypes mismatch"
        [ (UnQual (Ident "T"),[UnQual (Symbol ":+")])
        , (UnQual (Ident "R"),[UnQual (Ident "R"), UnQual (Ident "S")])
        , (UnQual (Ident "Maybe"),[UnQual (Ident "Nothing"), UnQual (Ident "Just")])
        ]
        (stateRecordTypes r)

case_importStateRecordTypes :: Assertion
case_importStateRecordTypes = do
  res <- compileFileWithState defConf "tests/Api/ImportRecords.hs"
  case res of
    Left err -> error (show err)
    Right (_,r) -> do
      -- TODO order should not matter
      assertEqual "stateRecordTypes mismatch"
        [ (UnQual (Ident "T"),[UnQual (Symbol ":+")])
        , (UnQual (Ident "R"),[UnQual (Ident "R"), UnQual (Ident "S")])
        , (UnQual (Ident "Maybe"),[UnQual (Ident "Nothing"), UnQual (Ident "Just")])
        ]
        (stateRecordTypes r)

fp :: FilePath
fp = "tests/RecordImport_Import.hs"

defConf :: CompileConfig
defConf = addConfigDirectoryInclude "tests/" $ def { configTypecheck = False }
