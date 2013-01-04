{-# LANGUAGE TemplateHaskell #-}

module Test.CommandLine (tests) where

import           Control.Applicative
import           Data.Maybe
import           System.Process.Extra
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.TH
import           Test.HUnit                     (Assertion, assertBool)
import           Test.Util

tests :: Test
tests = $testGroupGenerator

compileFile :: [String] -> IO (Either String String)
compileFile flags = do
  fay <- fromJust <$> fayPath
  r <- readAllFromProcess fay flags ""
  return $ case r of
    Left l -> Left l
    Right t -> Right $ snd t

case_executable :: Assertion
case_executable = do
  fay <- fayPath
  assertBool "Could not find fay executable" (isJust fay)

case_compile :: Assertion
case_compile = do
  res <- compileFile ["--include=tests", "tests/RecordImport_Import.hs","--no-ghc"]
  assertBool (fromLeft res) (isRight res)
