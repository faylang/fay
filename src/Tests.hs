{-# LANGUAGE ViewPatterns #-}

-- | Generate the web site/documentation for the Fay project.
--
-- This depends on the Fay compiler to generate examples and the
-- javascript of the page is also built with Fay.

module Main where

import           Fay
import           Fay.Compiler.Config

import           Control.Applicative
import           Data.Default
import           Data.List
import           Data.Maybe
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Process.Extra
import qualified Test.Api                       as Api
import qualified Test.CommandLine               as Cmd
import qualified Test.Convert                   as C
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     (assertEqual)

-- | Main test runner.
main :: IO ()
main = do
  sandbox <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  (packageConf,args) <- fmap (prefixed (=="-package-conf")) getArgs
  compiler <- makeCompilerTests (packageConf <|> sandbox)
  defaultMainWithArgs [Api.tests, Cmd.tests, compiler, C.tests]
                      args

-- | Extract the element prefixed by the given element in the list.
prefixed :: (a -> Bool) -> [a] -> (Maybe a,[a])
prefixed f (break f -> (x,y)) = (listToMaybe (drop 1 y),x ++ drop 2 y)

-- | Make the case-by-case unit tests.
makeCompilerTests :: Maybe FilePath -> IO Test
makeCompilerTests packageConf = do
  files <- fmap (map ("tests" </>) . sort . filter (isSuffixOf ".hs")) $ getDirectoryContents "tests"
  return $ testGroup "Tests" $ flip map files $ \file -> testCase file $ do
    testFile packageConf False file
    testFile packageConf True file

testFile :: Maybe FilePath -> Bool -> String -> IO ()
testFile packageConf opt file = do
  let root = (reverse . drop 1 . dropWhile (/='.') . reverse) file
      out = toJsName file
      config =
        addConfigDirectoryIncludePaths ["tests/"] $
          def { configOptimize = opt
              , configTypecheck = False
              , configPackageConf = packageConf
              }
  outExists <- doesFileExist root
  compileFromTo config file (Just out)
  result <- runJavaScriptFile out
  if outExists
     then do output <- readFile root
             assertEqual file output (either show id result)
     else assertEqual file True (either (const True) (const False) result)

-- | Run a JS file.
runJavaScriptFile :: String -> IO (Either String String)
runJavaScriptFile file = fmap (fmap snd) (readAllFromProcess "node" [file] "")
