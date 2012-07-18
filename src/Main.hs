{-# LANGUAGE ViewPatterns #-}

-- | Main compiler executable.

module Main where

import Language.Fay.Compiler

import Control.Monad
import Data.List
import System.Environment

-- | Main entry point.
main :: IO ()
main = do
  args <- getArgs
  let files = filter (not . isPrefixOf "-") args
      opts = map (drop 1) $ filter (isPrefixOf "-") args
  forM_ files $ \file -> do
    compileFromTo (elem "autorun" opts) file (toJsName file)
