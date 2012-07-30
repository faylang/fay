{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Fay.Compiler where

import           Control.Exception            (throw)
import           Language.Fay                 (compileModule, compileViaStr)
import           Language.Fay.Types
import           Language.Haskell.Exts.Syntax
import           Paths_fay

-- | Compile file program to…
compileFromTo :: CompileConfig -> Bool -> FilePath -> FilePath -> IO ()
compileFromTo config autorun filein fileout = do
  runtime <- getDataFileName "js/runtime.js"
  stdlibpath <- getDataFileName "hs/stdlib.hs"
  stdlibpathprelude <- getDataFileName "src/Language/Fay/Stdlib.hs"
  raw <- readFile runtime
  stdlib <- readFile stdlibpath
  stdlibprelude <- readFile stdlibpathprelude
  hscode <- readFile filein
  result <- compileProgram config
                           autorun
                           raw
                           compileModule
                           (hscode ++ "\n" ++ stdlib ++ "\n" ++ strip stdlibprelude)
  case result of
    Right out -> writeFile fileout out
    Left  err -> throw err

  where strip = unlines . dropWhile (/="-- START") . lines

-- | Compile the given module to a runnable program.
compileProgram :: (Show from,Show to,CompilesTo from to)
               => CompileConfig -> Bool -> String -> (from -> Compile to) -> String
               -> IO (Either CompileError String)
compileProgram config autorun raw with hscode = do
  result <- compileViaStr config with hscode
  case result of
    Left err -> return (Left err)
    Right (jscode,state) -> do
      let (ModuleName modulename) = stateModuleName state
          exports                 = stateExports state
      return (Right (unlines ["/** @constructor"
                             ,"*/"
                             ,"var " ++ modulename ++ " = function(){"
                             ,raw
                             ,jscode
                             ,"// Exports"
                             ,unlines (map printExport exports)
                             ,"// Built-ins"
                             ,"this.$force      = _;"
                             ,if configExportBuiltins config
                                 then unlines ["this.$           = $;"
                                              ,"this.$list       = Fay$$list;"
                                              ,"this.$encodeShow = Fay$$encodeShow;"
                                              ,"this.$eval       = Fay$$eval;"
                                              ]
                                 else ""
                             ,"};"
                             ,if autorun
                                 then unlines [";"
                                              ,"var main = new " ++ modulename ++ "();"
                                              ,"main.$force(main.main);"
                                              ]
                                 else ""
                             ]))

-- | Print an this.x = x; export out.
printExport :: Name -> String
printExport name =
  printJS (JsSetProp ":this"
                     (UnQual name)
                     (JsName (UnQual name)))

-- | Convert a Haskell filename to a JS filename.
toJsName :: String -> String
toJsName x = case reverse x of
               ('s':'h':'.': (reverse -> file)) -> file ++ ".js"
               _ -> x
