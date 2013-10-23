{-# LANGUAGE ViewPatterns #-}

-- | Generate the web site/documentation for the Fay project.
--
-- This depends on the Fay compiler to generate examples and the
-- javascript of the page is also built with Fay.

module Main where

import           Fay
import           Fay.Compiler.Config
import           Fay.System.Directory.Extra
import           Fay.System.Process.Extra

import           Control.Applicative
import           Data.Char
import           Data.Default
import           Data.List
import           Data.Maybe
import           Data.Ord
import           System.Directory
import           System.Environment
import           System.FilePath
import qualified Test.CommandLine               as Cmd
import qualified Test.Compile                   as Compile
import qualified Test.Convert                   as C
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     (assertEqual, assertFailure)

-- | Main test runner.
main :: IO ()
main = do
  sandbox <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  (packageConf,args) <- fmap (prefixed (=="-package-conf")) getArgs
  let (basePath,args') = prefixed (=="-base-path") args
  compiler <- makeCompilerTests (packageConf <|> sandbox) basePath
  defaultMainWithArgs [Compile.tests, Cmd.tests, compiler, C.tests]
                      args'

-- | Extract the element prefixed by the given element in the list.
prefixed :: (a -> Bool) -> [a] -> (Maybe a,[a])
prefixed f (break f -> (x,y)) = (listToMaybe (drop 1 y),x ++ drop 2 y)

-- | Make the case-by-case unit tests.
makeCompilerTests :: Maybe FilePath -> Maybe FilePath -> IO Test
makeCompilerTests packageConf basePath = do
  files <- sortBy (comparing (map toLower)) . filter (\v -> not (isInfixOf "/Compile/" v) && not (isInfixOf "/regressions/" v)) . filter (isSuffixOf ".hs") <$> getRecursiveContents "tests"
  return $ testGroup "Tests" $ flip map files $ \file -> testCase file $ do
    testFile packageConf basePath False file
    testFile packageConf basePath True file

testFile :: Maybe FilePath -> Maybe FilePath -> Bool -> String -> IO ()
testFile packageConf basePath opt file = do
  let root = (reverse . drop 1 . dropWhile (/='.') . reverse) file
      out = toJsName file
      resf = root <.> "res"
      config =
        addConfigDirectoryIncludePaths ["tests/"] $
          def { configOptimize = opt
              , configTypecheck = False
              , configPackageConf = packageConf
              , configBasePath = basePath
              }
  resExists <- doesFileExist resf
  let partialName = root ++ "_partial.res"
  partialExists <- doesFileExist partialName
  compileFromTo config file (Just out)
  result <- runJavaScriptFile out
  if resExists
     then do output <- readFile resf
             assertEqual file output (either show snd result)
     else
       if partialExists
         then case result of
           Left (_,res) -> do
             output <- readFile partialName
             assertEqual file output res
           Right (err,res) -> assertFailure $ "Did not fail:\n stdout: " ++ res ++ "\n\nstderr: " ++ err
         else assertEqual (file ++ ": Expected program to fail") True (either (const True) (const False) result)

-- | Run a JS file.
runJavaScriptFile :: String -> IO (Either (String,String) (String,String))
runJavaScriptFile file = readAllFromProcess "node" [file] ""
