module Main where

import Language.Fay.Compiler

import Data.List
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Test.HUnit

-- | Main test runner.
main :: IO ()
main = runUnitTests >>= print

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
              compileFromTo True file out
              result <- runJavaScriptFile out
              if outExists
                 then do output <- readFile root
                         assertEqual file output (either show id result)
                 else assertEqual file True (either (const True) (const False) result)

-- | Run a JS file.
runJavaScriptFile :: String -> IO (Either String String)
runJavaScriptFile file = readAllFromProcess "node" file

-- | Read all stuff from a process.
readAllFromProcess :: FilePath -> String -> IO (Either String String)
readAllFromProcess program file = do
  (_,out,err,pid) <- runInteractiveProcess program [file] Nothing Nothing
  code <- waitForProcess pid
  case code of
    ExitSuccess -> fmap Right (hGetContents out)
    ExitFailure _ -> fmap Left (hGetContents err)
