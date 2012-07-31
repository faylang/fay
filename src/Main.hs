
-- | Main compiler executable.

module Main where

import           Language.Fay.Compiler
import           Language.Fay.Types

import           Control.Monad
import           Data.Default
import           Data.List
import           System.Environment

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  let files = filter (not . isPrefixOf "-") args
      opts = map (drop 1) $ filter (isPrefixOf "-") args
  if (elem "help" opts) || null files
    then putStrLn helpText
    else forM_ files $ \file -> do
      compileFromTo def { configTCO = elem "tco" opts
                        , configInlineForce = elem "inline-force" opts
                        , configFlattenApps = elem "flatten-apps" opts
                        , configExportBuiltins = not (elem "no-export-builtins" opts)
                        }
        (elem "autorun" opts)
        file
        (toJsName file)


helpText = unlines
  ["fay -- compiler from (a proper subset of) Haskell to JavaScript"
  ,""
  ,"USAGE"
  ,"  fay [OPTIONS] <hs-input-file> ... "
  ,""
  ,"OPTIONS"
  ,"  -autorun    automatically call main in generated JavaScript"
  ]
