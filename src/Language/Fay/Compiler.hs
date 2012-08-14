{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Fay.Compiler where

import           Control.Applicative
import           Control.Exception            (throw)
import           Control.Monad
import           Language.Fay                 (compileToplevelModule, compileViaStr, prettyPrintString)
import           Language.Fay.Types
import           Language.Haskell.Exts.Syntax
import           Paths_fay
import           System.FilePath
import           Text.Groom


class Writer a where
  writeout :: a -> String -> IO ()

class Reader a where
  readin :: a -> IO String

instance Writer FilePath where
  writeout = writeFile

instance Reader FilePath where
  readin = readFile
-- | Compile file program to…
compileFromTo :: CompileConfig -> FilePath -> FilePath -> IO ()
compileFromTo config filein fileout = do
  result <- compileFile config filein
  case result of
    Right out -> do
      writeFile fileout out
      when (configHtmlWrapper config) $
        writeFile (replaceExtension fileout "html") $ unlines [
            "<html>"
          , "  <head>"
          , "    <script type=\"text/javascript\" src=\"" ++ relativeJsPath ++ "\">"
          , "    </script>"
          , "  </head>"
          , "  <body>"
          , "  </body>"
          , "</html>"] where relativeJsPath = makeRelative (dropFileName fileout) fileout
    Left err -> error . groom $ err

compileReadWrite :: (Reader r, Writer w) => CompileConfig -> r -> w -> IO ()
compileReadWrite config reader writer = do
  result <- compileFile config reader
  case result of
    Right out -> do
      writeout writer out
    Left err -> error . groom $ err

compileFile :: (Reader r) => CompileConfig -> r -> IO (Either CompileError String)
compileFile config filein = do
  runtime <- getDataFileName "js/runtime.js"
  stdlibpath <- getDataFileName "hs/stdlib.hs"
  stdlibpathprelude <- getDataFileName "src/Language/Fay/Stdlib.hs"
  raw <- readFile runtime
  stdlib <- readFile stdlibpath
  stdlibprelude <- readFile stdlibpathprelude
  hscode <- readin filein
  compileProgram config
                 raw
                 compileToplevelModule
                 (hscode ++ "\n" ++ stdlib ++ "\n" ++ strip stdlibprelude)

  where strip = unlines . dropWhile (/="-- START") . lines

-- | Compile the given module to a runnable program.
compileProgram :: (Show from,Show to,CompilesTo from to)
               => CompileConfig -> String -> (from -> Compile to) -> String
               -> IO (Either CompileError String)
compileProgram config raw with hscode = do
  result <- compileViaStr config with hscode
  case result of
    Left err -> return (Left err)
    Right (jscode,state) -> do
      let (ModuleName modulename) = stateModuleName state
          exports                 = stateExports state
      let result = (Right (unlines ["/** @constructor"
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
                             ,if configAutorun config
                                 then unlines [";"
                                              ,"var main = new " ++ modulename ++ "();"
                                              ,"main._(main.main);"
                                              ]
                                 else ""
                             ]))
      case result of
        l@(Left _) -> return l
        Right s -> if configPrettyPrint config
                     then Right <$> prettyPrintString s
                     else return $ Right s

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
