{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Fay
  (module Language.Fay.Types
  ,compileFile
  ,compileFromTo
  ,compileFromToAndGenerateHtml
  ,toJsName
  ,showCompileError)
   where

import Language.Fay.Compiler    (compileToplevelModule, compileViaStr)
import Language.Fay.Print
import Language.Fay.Types

import Control.Monad
import Data.List
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts (prettyPrint)
import Paths_fay
import System.FilePath

-- | Compile the given file and write the output to the given path, or
-- if nothing given, stdout.
compileFromTo :: CompileConfig -> FilePath -> Maybe FilePath -> IO ()
compileFromTo config filein fileout = do
  result <- maybe (compileFile config filein)
                  (compileFromToAndGenerateHtml config filein)
                  fileout
  case result of
    Right out -> maybe (putStrLn out) (flip writeFile out) fileout
    Left err -> error $ showCompileError $ err

-- | Compile the given file and write to the output, also generate any HTML.
compileFromToAndGenerateHtml :: CompileConfig -> FilePath -> FilePath -> IO (Either CompileError String)
compileFromToAndGenerateHtml config filein fileout = do
  result <- compileFile config { configFilePath = Just filein } filein
  case result of
    Right out -> do
      when (configHtmlWrapper config) $
        writeFile (replaceExtension fileout "html") $ unlines [
            "<!doctype html>"
          , "<html>"
          , "  <head>"
          ,"    <meta http-equiv='Content-Type' content='text/html; charset=utf-8'>"
          , unlines . map ("    "++) . map makeScriptTagSrc $ configHtmlJSLibs config
          , "    " ++ makeScriptTagSrc relativeJsPath
          , "    </script>"
          , "  </head>"
          , "  <body>"
          , "  </body>"
          , "</html>"]
      return (Right out)
            where relativeJsPath = makeRelative (dropFileName fileout) fileout
                  makeScriptTagSrc :: FilePath -> String
                  makeScriptTagSrc = \s ->
                    "<script type=\"text/javascript\" src=\"" ++ s ++ "\"></script>"
    Left err -> return (Left err)

-- | Compile the given file.
compileFile :: CompileConfig -> FilePath -> IO (Either CompileError String)
compileFile config filein = do
  runtime <- getDataFileName "js/runtime.js"
  stdlibpath <- getDataFileName "hs/stdlib.hs"
  stdlibpathprelude <- getDataFileName "src/Language/Fay/Stdlib.hs"
  raw <- readFile runtime
  stdlib <- readFile stdlibpath
  stdlibprelude <- readFile stdlibpathprelude
  hscode <- readFile filein
  compileToModule filein
                  config
                  raw
                  compileToplevelModule
                  (hscode ++ "\n" ++ stdlib ++ "\n" ++ strip stdlibprelude)

  where strip = unlines . dropWhile (/="-- START") . lines

-- | Compile the given module to a runnable module.
compileToModule :: (Show from,Show to,CompilesTo from to)
               => FilePath
               -> CompileConfig -> String -> (from -> Compile to) -> String
               -> IO (Either CompileError String)
compileToModule filepath config raw with hscode = do
  result <- compileViaStr filepath config with hscode
  case result of
    Left err -> return (Left err)
    Right (PrintState{..},state) ->
      return $ Right $ (generate (concat (reverse psOutput))
                                 (stateExports state)
                                 (stateModuleName state))

  where generate jscode exports (ModuleName (clean -> modulename)) = unlines
          ["/** @constructor"
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
          ,if not (configLibrary config)
              then unlines [";"
                           ,"var main = new " ++ modulename ++ "();"
                           ,"main._(main.main);"
                           ]
              else ""
          ]
        clean ('.':cs) = '$' : clean cs
        clean (c:cs)   = c : clean cs
        clean [] = []

-- | Print an this.x = x; export out.
printExport :: QName -> String
printExport name =
  printJSString (JsSetProp JsThis
                           (JsNameVar name)
                           (JsName (JsNameVar name)))

-- | Convert a Haskell filename to a JS filename.
toJsName :: String -> String
toJsName x = case reverse x of
               ('s':'h':'.': (reverse -> file)) -> file ++ ".js"
               _ -> x

-- | Print a compile error for human consumption.
showCompileError :: CompileError -> String
showCompileError e =
  case e of
    ParseError _ err -> err
    UnsupportedDeclaration d -> "unsupported declaration: " ++ prettyPrint d
    UnsupportedExportSpec es -> "unsupported export specification: " ++ prettyPrint es
    UnsupportedMatchSyntax m -> "unsupported match/binding syntax: " ++ prettyPrint m
    UnsupportedWhereInMatch m -> "unsupported `where' syntax: " ++ prettyPrint m
    UnsupportedExpression expr -> "unsupported expression syntax: " ++ prettyPrint expr
    UnsupportedQualStmt stmt -> "unsupported list qualifier: " ++ prettyPrint stmt
    UnsupportedLiteral lit -> "unsupported literal syntax: " ++ prettyPrint lit
    UnsupportedLetBinding d -> "unsupported let binding: " ++ prettyPrint d
    UnsupportedOperator qop -> "unsupported operator syntax: " ++ prettyPrint qop
    UnsupportedPattern pat -> "unsupported pattern syntax: " ++ prettyPrint pat
    UnsupportedRhs rhs -> "unsupported right-hand side syntax: " ++ prettyPrint rhs
    UnsupportedGuardedAlts ga -> "unsupported guarded alts: " ++ prettyPrint ga
    EmptyDoBlock -> "empty `do' block"
    UnsupportedModuleSyntax{} -> "unsupported module syntax (may be supported later)"
    LetUnsupported -> "let not supported here"
    InvalidDoBlock -> "invalid `do' block"
    RecursiveDoUnsupported -> "recursive `do' isn't supported"
    FfiNeedsTypeSig d -> "your FFI declaration needs a type signature: " ++ prettyPrint d
    FfiFormatBadChars cs -> "invalid characters for FFI format string: " ++ show cs
    FfiFormatNoSuchArg i -> "no such argument in FFI format string: " ++ show i
    FfiFormatIncompleteArg -> "incomplete `%' syntax in FFI format string"
    FfiFormatInvalidJavaScript code err -> "invalid JavaScript code in FFI format string:\n"
                                           ++ err ++ "\nin " ++ code
    UnsupportedFieldPattern p -> "unsupported field pattern: " ++ prettyPrint p
    UnsupportedImport i -> "unsupported import syntax, we're too lazy: " ++ prettyPrint i
    Couldn'tFindImport i places ->
      "could not find an import in the path: " ++ prettyPrint i ++ ", \n" ++
      "searched in these places: " ++ intercalate ", " places
