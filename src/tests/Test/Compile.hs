{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Compile (tests,runScriptFile) where

import           Fay
import           Fay.Compiler.Prelude
#if !MIN_VERSION_base(4,7,0)
import           Test.Util                       (isRight)
#endif

import           Language.Haskell.Exts
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH

tests :: TestTree
tests = $testGroupGenerator

case_imports :: Assertion
case_imports = do
  cfg <- defConf
  res <- compileFile cfg fp
  assertBool "Could not compile file with imports" (isRight res)

case_importedList :: Assertion
case_importedList = do
  cfg <- defConf
  res <- compileFileWithState cfg fp
  case res of
    Left err -> error (show err)
    Right (_,_,r) -> assertBool "RecordImport_Export was not added to stateImported" .
                       isJust . lookup (ModuleName () "RecordImport_Export") $ stateImported r

fp :: FilePath
fp = "tests/RecordImport_Import.hs"

case_stateRecordTypes :: Assertion
case_stateRecordTypes = do
  cfg <- defConf
  res <- compileFileWithState cfg "tests/Compile/Records.hs"
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
  cfg <- defConf
  res <- compileFileWithState cfg "tests/Compile/ImportRecords.hs"
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
  cfg <- defConf
  res <- compileFile cfg { configTypecheck = True, configFilePath = Just "tests/Compile/CPPTypecheck.hs" } "tests/Compile/CPPTypecheck.hs"
  either (assertFailure . show) (const $ return ()) res

case_cppMultiLineStrings :: Assertion
case_cppMultiLineStrings = do
  cfg <- defConf
  res <- compileFile cfg { configTypecheck = True, configFilePath = Just "tests/Compile/CPPMultiLineStrings.hs" } "tests/Compile/CPPMultiLineStrings.hs"
  either (assertFailure . show) (const $ return ()) res

case_strictWrapper :: Assertion
case_strictWrapper = do
  cfg <- defConf
  res <- compileFile cfg { configTypecheck = True, configFilePath = Just "tests/Compile/StrictWrapper.hs", configStrict = ["StrictWrapper"] } "tests/Compile/StrictWrapper.hs"
  let suffix = if configTypeScript cfg then ".ts" else ".js"
  (\a b -> either a b res) (assertFailure . show) $ \js -> do
    writeFile ("tests/Compile/StrictWrapper" ++ suffix) js
    (err, out) <- either id id <$> runScriptFile ("tests/Compile/StrictWrapper" ++ suffix)
    when (err /= "") $ assertFailure err
    expected <- readFile "tests/Compile/StrictWrapper.res"
    assertEqual "strictWrapper node stdout" expected out

assertPretty :: Config -> String -> Assertion
assertPretty cfg flagName = do
  let suffix = if configTypeScript cfg then ".ts" else ".js"
  res <- compileFile cfg $ "tests/Compile/" ++ flagName ++ ".hs"
  case res of
    Left l  -> assertFailure $ "Should compile, but failed with: " ++ show l
    Right js -> do
    writeFile ("tests/Compile/" ++ flagName ++ suffix) js
    (err, out) <- either id id <$> runScriptFile ("tests/Compile/" ++ flagName ++ suffix)
    when (err /= "") $ assertFailure err
    expected <- readFile $ "tests/Compile/" ++ flagName ++ ".res"
    assertEqual (flagName ++ " node stdout") expected out

case_pretty :: Assertion
case_pretty = do
  cfg <- defConf
  assertPretty cfg { configPrettyPrint = True } "pretty"

case_prettyThunks :: Assertion
case_prettyThunks = do
  cfg <- defConf
  assertPretty cfg { configPrettyThunks = True } "prettyThunks"

case_prettyOperators :: Assertion
case_prettyOperators = do
  cfg <- defConf
  assertPretty cfg { configPrettyOperators = True } "prettyOperators"

case_charEnum :: Assertion
case_charEnum = do
  cfg <- defConf
  res <- compileFile cfg { configTypecheck = True, configFilePath = Just "tests/Compile/EnumChar.hs" } "tests/Compile/EnumChar.hs"
  case res of
    Left UnsupportedEnum{} -> return ()
    Left l  -> assertFailure $ "Should have failed with UnsupportedEnum, but failed with: " ++ show l
    Right _ -> assertFailure "Should have failed with UnsupportedEnum, but compiled"

defConf :: IO Config
defConf = do
  cfg <- defaultConfigWithSandbox
#if TYPESCRIPT
  return $ addConfigDirectoryIncludePaths ["tests/"] cfg { configTypecheck = False, configTypeScript = True }
#else
  return $ addConfigDirectoryIncludePaths ["tests/"] cfg { configTypecheck = False }
#endif

-- | Run a JS or TS file.
runScriptFile :: String -> IO (Either (String,String) (String,String))
runScriptFile file = do
#if TYPESCRIPT
  tsc_ret <- readAllFromProcess "tsc" [file] ""
  case tsc_ret of
    Left _ -> return tsc_ret
    Right _ -> readAllFromProcess "node" [(reverse (drop 3 (reverse file))) ++ ".js" ] ""
#else
  readAllFromProcess "node" [file] ""
#endif
