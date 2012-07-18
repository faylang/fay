{-# LANGUAGE ViewPatterns #-}

-- | Main compiler executable.

module Main where

import Language.Fay                 hiding (throw)
import Paths_fay

import Control.Exception  (throw)
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

  where toJsName x = case reverse x of
                       ('s':'h':'.': (reverse -> file)) -> file ++ ".js"
                       _ -> x

-- | Compile file program to…
compileFromTo autorun filein fileout = do
  runtime <- getDataFileName "js/runtime.js"
  stdlibpath <- getDataFileName "hs/stdlib.hs"
  raw <- readFile runtime
  stdlib <- readFile stdlibpath
  hscode <- readFile filein
  result <- compileProgram autorun raw compileModule (hscode ++ stdlib)
  case result of
    Right out -> writeFile fileout out
    Left  err -> throw err

-- | Compile the given module to a runnable program.
compileProgram autorun raw with hscode = do
  result <- compileViaStr with hscode
  case result of
    Left err -> return (Left err)
    Right jscode -> return (Right (unlines ["var Fay = function(){"
                                           ,raw
                                           ,jscode
                                           ,"return {"
                                           ,"  force:_,"
                                           ,"  thunk:Fay$$Thunk,"
                                           ,"  list:Fay$$list,"
                                           ,"  encodeShow:Fay$$encodeShow,"
                                           ,"  main:main,"
                                           ,"  eval:Fay$$eval"
                                           ,"  };"
                                           ,"};"
                                           ,if autorun
                                               then ";\nvar fay = new Fay();fay.force(fay.main);"
                                               else ""
                                           ]))
