-- | Generate the web site/documentation for the Fay project.
--
-- This depends on the Fay compiler to generate examples and the
-- javascript of the page is also built with Fay.

module Main where

import           Data.Default
import           Data.List
import           Language.Fay
import           System.Directory
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
  compiler <- makeCompilerTests
  defaultMain [Api.tests, Cmd.tests, compiler, C.tests]

-- | Make the case-by-case unit tests.
makeCompilerTests :: IO Test
makeCompilerTests = do
  files <- fmap (map ("tests" </>) . sort . filter dotHs) $ getDirectoryContents "tests"
  return $ testGroup "Tests" $ flip map files $ \file ->
    testCase file $ do
      let root = (reverse . drop 1 . dropWhile (/='.') . reverse) file
          out = toJsName file
      outExists <- doesFileExist root
      compileFromTo def { configTypecheck = False, configDirectoryIncludes = ["tests/"] } file (Just out)
      result <- runJavaScriptFile out
      if outExists
         then do output <- readFile root
                 assertEqual file output (either show id result)
         else assertEqual file True (either (const True) (const False) result)
  where dotHs = isSuffixOf ".hs"

-- | Run a JS file.
runJavaScriptFile :: String -> IO (Either String String)
runJavaScriptFile file = readAllFromProcess "node" file
