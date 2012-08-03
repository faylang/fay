{-# LANGUAGE TemplateHaskell #-}
-- | Main compiler executable.

module Main where

import           Language.Fay.Compiler
import           Language.Fay.Types

import           Control.Monad
import           Data.Default
import           Data.List
import           Options
import           System.Environment

defineOptions "FayCompilerOptions" $ do
  boolOption "optStdout" "stdout" False ""

-- | Main entry point.
main :: IO ()
main = runCommand $ \opts args -> do
  if optStdout opts
     then print "stdout"
     else print "done"
  print $ show args
-- main = do
--   args <- getArgs
--   let files = filter (not . isPrefixOf "-") args
--       opts = map (drop 1) $ filter (isPrefixOf "-") args
--   if (elem "help" opts) || null files
--     then putStrLn helpText
--     else forM_ files $ \file -> do
--       compileFromTo def { configTCO = elem "tco" opts
--                         , configInlineForce = elem "inline-force" opts
--                         , configFlattenApps = elem "flatten-apps" opts
--                         , configExportBuiltins = not (elem "no-export-builtins" opts)
--                         }
--         (elem "autorun" opts)
--         file
--         (toJsName file)


-- helpText = unlines
--   ["fay -- compiler from (a proper subset of) Haskell to JavaScript"
--   ,""
--   ,"USAGE"
--   ,"  fay [OPTIONS] <hs-input-file> ... "
--   ,""
--   ,"OPTIONS"
--   ,"  -autorun       automatically call main in generated JavaScript"
--   ,"  -inline-force  inline forcing, adds some speed for numbers, blows up code a bit"
--   ,"  -flatten-apps  flatten function application, can be more readable,"
--   ,"                 no noticeable speed difference"
--   ]
