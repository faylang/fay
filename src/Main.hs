{-# LANGUAGE ViewPatterns #-}

-- | Main compiler executable.

module Main where

import Language.Fay.Compiler
import Language.Fay.Types

import Control.Monad
import Data.Default
import Data.List
import System.Environment

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  let files = filter (not . isPrefixOf "-") args
      opts = map (drop 1) $ filter (isPrefixOf "-") args
  forM_ files $ \file -> do
    compileFromTo def { configTCO = elem "tco" opts
                      , configInlineForce = elem "inline-force" opts
                      }
                  (elem "autorun" opts)
                  file
                  (toJsName file)
