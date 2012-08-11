{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Fay.Compiler where

import           Control.Exception            (throw)
import           Control.Monad
import           Language.Fay                 (compileModule, compileViaStr)
import           Language.Fay.Types
import           Language.Haskell.Exts.Syntax
import           Paths_fay
import           System.FilePath

type Writer = String -> IO ()
type Reader = IO String

-- | Compile file program to…
compileFromTo :: CompileConfig -> Bool -> Bool -> FilePath -> FilePath -> IO ()
compileFromTo config autorun htmlWrapper filein fileout = do
  result <- compileFile config autorun filein
  case result of
    Right out -> do
      writeFile fileout out
      when htmlWrapper $
        writeFile (replaceExtension fileout "html") $ unlines [
            "<html>"
          , "  <head>"
          , "    <script type=\"text/javascript\" src=\"" ++ relativeJsPath ++ "\">"
          , "    </script>"
          , "  </head>"
          , "  <body>"
          , "  </body>"
          , "</html>"] where relativeJsPath = makeRelative (dropFileName fileout) fileout
    Left err -> throw err

compileFile :: CompileConfig -> Bool -> FilePath -> IO (Either CompileError String)
compileFile config autorun filein = do
  runtime <- getDataFileName "js/runtime.js"
  stdlibpath <- getDataFileName "hs/stdlib.hs"
  stdlibpathprelude <- getDataFileName "src/Language/Fay/Stdlib.hs"
  raw <- readFile runtime
  stdlib <- readFile stdlibpath
  stdlibprelude <- readFile stdlibpathprelude
  hscode <- readFile filein
  compileProgram config
                 autorun
                 raw
                 compileModule
                 (hscode ++ "\n" ++ stdlib ++ "\n" ++ strip stdlibprelude)


  where strip = unlines . dropWhile (/="-- START") . lines

-- =======
-- compileFromTo :: CompileConfig -> Bool -> FilePath -> FilePath -> IO ()
-- compileFromTo config autorun filein fileout = compile config autorun reader writer
--   where writer = writeFile fileout
--         reader = readFile filein

compile :: CompileConfig -> Bool -> Reader -> Writer -> IO ()
compile config autorun reader writer = do
  runtime <- getDataFileName "js/runtime.js"
  stdlibpath <- getDataFileName "hs/stdlib.hs"
  stdlibpathprelude <- getDataFileName "src/Language/Fay/Stdlib.hs"
  raw <- readFile runtime
  stdlib <- readFile stdlibpath
  stdlibprelude <- readFile stdlibpathprelude
  hscode <- reader
  result <- compileProgram config
                           autorun
                           raw
                           compileModule
                           (hscode ++ "\n" ++ stdlib ++ "\n" ++ strip stdlibprelude)
  case result of
    Right out -> writer out
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
                             ,"this._ = _;"
                             ,if configExportBuiltins config
                                 then unlines ["this.$           = $;"
                                              ,"this.$fayToJs    = Fay$$fayToJs;"
                                              ,"this.$jsToFay    = Fay$$jsToFay;"
                                              ]
                                 else ""
                             ,"};"
                             ,if autorun
                                 then unlines [";"
                                              ,"var main = new " ++ modulename ++ "();"
                                              ,"main._(main.main);"
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
