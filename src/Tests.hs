-- | Generate the web site/documentation for the Fay project.
--
-- This depends on the Fay compiler to generate examples and the
-- javascript of the page is also built with Fay.

module Main where

import           Language.Fay.Compiler
import           Language.Fay.Types

import           Control.Monad
import           Data.Default
import           Data.List
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           System.Process.Extra
import           Test.HUnit

-- | Main test runner.
main :: IO ()
main = void runUnitTests

-- | Run the case-by-case unit tests.
runUnitTests :: IO Counts
runUnitTests = do
  files <- fmap (map ("tests" </>) . sort . filter dotHs) $ getDirectoryContents "tests"
  runTestTT (makeTests files)

    where dotHs = isSuffixOf ".hs"
          makeTests files =
            TestList $ flip map files $ \file -> TestLabel file $ TestCase $ do
              let root = (reverse . drop 1 . dropWhile (/='.') . reverse) file
                  out = toJsName file
              outExists <- doesFileExist root
              compileFromTo def { configAutorun = True, configDirectoryIncludes = ["tests/"] } file out
              result <- runJavaScriptFile out
              if outExists
                 then do output <- readFile root
                         assertEqual file output (either show id result)
                 else assertEqual file True (either (const True) (const False) result)

-- | Run a JS file.
runJavaScriptFile :: String -> IO (Either String String)
runJavaScriptFile file = readAllFromProcess "node" file
