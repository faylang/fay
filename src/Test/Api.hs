{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Api (tests) where

import           Language.Fay                   hiding (compileFile)
import           Language.Fay.Compiler
import           Paths_fay

import           Data.Default
import           Data.Maybe
import           Language.Haskell.Exts.Syntax
import           System.FilePath
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit                     (Assertion, assertBool)
import           Test.Util

tests :: Test
tests = $testGroupGenerator

case_imports :: Assertion
case_imports = do
  res <- compileFile defConf fp
  assertBool "Could not compile file with imports" (isRight res)

case_importedList :: Assertion
case_importedList = do
  res <- compileFile defConf fp
  case res of
    Left err -> error (show err)
    Right r -> assertBool "RecordImport_Export was not added to stateImported" $
                 isJust $ lookup (ModuleName "RecordImport_Export") (stateImported r)

fp :: FilePath
fp = "tests/RecordImport_Import.hs"

defConf :: CompileConfig
defConf = def { configTypecheck = False, configDirectoryIncludes = ["tests"] }

compileFile :: CompileConfig -> FilePath -> IO (Either CompileError CompileState)
compileFile config filein = do
  srcdir <- fmap (takeDirectory . takeDirectory . takeDirectory) (getDataFileName "src/Language/Fay/Stdlib.hs")
  hscode <- readFile filein
  result <- compileViaStr filein
    (config { configDirectoryIncludes = configDirectoryIncludes config ++ [srcdir] })
    compileToplevelModule hscode
  return $ either Left (Right . snd) result
