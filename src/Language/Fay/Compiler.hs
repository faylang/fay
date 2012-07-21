{-# LANGUAGE ViewPatterns #-}
module Language.Fay.Compiler where

import Language.Fay
import Language.Fay.Types
import Paths_fay

import Control.Exception  (throw)

-- | Compile file program to…
compileFromTo :: Bool -> FilePath -> FilePath -> IO ()
compileFromTo autorun filein fileout = do
  runtime <- getDataFileName "js/runtime.js"
  stdlibpath <- getDataFileName "hs/stdlib.hs"
  stdlibpathprelude <- getDataFileName "src/Language/Fay/Stdlib.hs"
  raw <- readFile runtime
  stdlib <- readFile stdlibpath
  stdlibprelude <- readFile stdlibpathprelude
  hscode <- readFile filein
  result <- compileProgram autorun raw compileModule (hscode ++ "\n" ++ stdlib ++ "\n" ++ strip stdlibprelude)
  case result of
    Right out -> writeFile fileout out
    Left  err -> throw err
      
  where strip = unlines . dropWhile (/="-- START") . lines

-- | Compile the given module to a runnable program.
compileProgram :: (Show from,Show to,CompilesTo from to)
               => Bool -> String -> (from -> Compile to) -> String
               -> IO (Either CompileError String)
compileProgram autorun raw with hscode = do
  result <- compileViaStr with hscode
  case result of
    Left err -> return (Left err)
    Right jscode -> return (Right (unlines ["var Fay = function(){"
                                           ,raw
                                           ,jscode
                                           ,"return {"
                                           ,"  force:_,"
                                           ,"  thunk:$,"
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

-- | Convert a Haskell filename to a JS filename.
toJsName :: String -> String
toJsName x = case reverse x of
               ('s':'h':'.': (reverse -> file)) -> file ++ ".js"
               _ -> x
