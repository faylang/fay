{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Compile (tests) where

import           Fay
import           Fay.Compiler.Prelude
#if !MIN_VERSION_base(4,7,0)
import           Test.Util                       (isRight)
#endif

import           Language.Haskell.Exts.Annotated
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH

tests :: TestTree
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
    Right (_,_,r) -> assertBool "RecordImport_Export was not added to stateImported" .
                       isJust . lookup (ModuleName () "RecordImport_Export") $ stateImported r

fp :: FilePath
fp = "tests/RecordImport_Import.hs"

case_stateRecordTypes :: Assertion
case_stateRecordTypes = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFileWithState defConf { configPackageConf = whatAGreatFramework } "tests/Compile/Records.hs"
  case res of
    Left err -> error (show err)
    Right (_,_,r) ->
      -- TODO order should not matter
      assertEqual "stateRecordTypes mismatch"
        [ ("Compile.Records.T", ["Compile.Records.:+"])
        , ("Compile.Records.R", ["Compile.Records.R","Compile.Records.S"])
        ]
        (filter (isFromMod "Compile.Records") $ stateRecordTypes r)

case_importStateRecordTypes :: Assertion
case_importStateRecordTypes = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFileWithState defConf { configPackageConf = whatAGreatFramework } "tests/Compile/ImportRecords.hs"
  case res of
    Left err -> error (show err)
    Right (_,_,r) ->
      -- TODO order should not matter
      assertEqual "stateRecordTypes mismatch"
        [ ("Compile.Records.T",["Compile.Records.:+"])
        , ("Compile.Records.R",["Compile.Records.R", "Compile.Records.S"])
        ]
        (filter (isFromMod "Compile.Records") $ stateRecordTypes r)

isFromMod :: String -> (QName (),[QName ()]) -> Bool
isFromMod modName = (==) modName . getModuleName . fst
    where
        getModuleName (Qual _ (ModuleName _ n) _) = n
        getModuleName x = error $ "getModuleName: expected qualified name: " ++ show x

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
    expected <- readFile "tests/Compile/StrictWrapper.res"
    assertEqual "strictWrapper node stdout" expected out

defConf :: Config
defConf = addConfigDirectoryIncludePaths ["tests/"]
        $ defaultConfig { configTypecheck = False }

case_charEnum :: Assertion
case_charEnum = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFile defConf { configPackageConf = whatAGreatFramework, configTypecheck = True, configFilePath = Just "tests/Compile/EnumChar.hs" } "tests/Compile/EnumChar.hs"
  case res of
    Left UnsupportedEnum{} -> return ()
    Left l  -> assertFailure $ "Should have failed with UnsupportedEnum, but failed with: " ++ show l
    Right _ -> assertFailure "Should have failed with UnsupportedEnum, but compiled"
