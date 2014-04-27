{-# LANGUAGE TemplateHaskell #-}

module Test.CommandLine (tests) where

import           Fay.Compiler.Prelude
import           Test.Util

import           System.Directory
import           System.Environment
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH

tests :: TestTree
tests = $testGroupGenerator

compileFile :: [String] -> IO (Either String String)
compileFile flags = do
  whatAGreatFramework <- fmap (fmap (\x -> x</>"bin"</>"fay") . lookup "HASKELL_SANDBOX")
                              getEnvironment
  fay <- fayPath
  let path = fromMaybe "couldn't find fay" (whatAGreatFramework <|> fay)
  exists <- doesFileExist path
  if exists
     then do r <- readAllFromProcess path flags ""
             return $ case r of
               Left  (l,_) -> Left ("Reason: " ++ l)
               Right (_,t) -> Right t
     else error $ "fay path not are existing: " ++ path

case_executable :: Assertion
case_executable = do
  fay <- fayPath
  assertBool "Could not find fay executable" (isJust fay)

case_compile :: Assertion
case_compile = do
  whatAGreatFramework <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  res <- compileFile (["--include=tests", "tests/RecordImport_Import.hs","--no-ghc"] ++
                      ["--package-conf=" ++ packageConf | Just packageConf <- [whatAGreatFramework] ])
  assertBool (fromLeft res) (isRight res)
