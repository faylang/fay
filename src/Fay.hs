{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Main library entry point.

module Fay
  (module Fay.Types
  ,compileFile
  ,compileFileWithState
  ,compileFromTo
  ,compileFromToAndGenerateHtml
  ,toJsName
  ,showCompileError
  ,getRuntime)
   where

import           Fay.Compiler
import           Fay.Compiler.Misc                      (printSrcSpanInfo)
import           Fay.Compiler.Packages
import qualified Fay.Exts                               as F
import           Fay.Types

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Language.Haskell.Exts.Annotated        (prettyPrint)
import           Language.Haskell.Exts.Annotated.Syntax
import           Language.Haskell.Exts.SrcLoc
import           Paths_fay
import           System.FilePath

-- | Compile the given file and write the output to the given path, or
-- if nothing given, stdout.
compileFromTo :: CompileConfig -> FilePath -> Maybe FilePath -> IO ()
compileFromTo config filein fileout = do
  result <- maybe (compileFile config filein)
                  (compileFromToAndGenerateHtml config filein)
                  fileout
  case result of
    Right out -> maybe (putStrLn out) (`writeFile` out) fileout
    Left err -> error $ showCompileError err

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
          , unlines . map (("    "++) . makeScriptTagSrc) $ configHtmlJSLibs config
          , "    " ++ makeScriptTagSrc relativeJsPath
          , "    </script>"
          , "  </head>"
          , "  <body>"
          , "  </body>"
          , "</html>"]
      return (Right out)
            where relativeJsPath = makeRelative (dropFileName fileout) fileout
                  makeScriptTagSrc :: FilePath -> String
                  makeScriptTagSrc s = "<script type=\"text/javascript\" src=\"" ++ s ++ "\"></script>"
    Left err -> return (Left err)

-- | Compile the given file.
compileFile :: CompileConfig -> FilePath -> IO (Either CompileError String)
compileFile config filein = either Left (Right . fst) <$> compileFileWithState config filein

-- | Compile a file returning the state.
compileFileWithState :: CompileConfig -> FilePath -> IO (Either CompileError (String,CompileState))
compileFileWithState config filein = do
  runtime <- getRuntime
  hscode <- readFile filein
  raw <- readFile runtime
  config' <- resolvePackages config
  compileToModule filein config' raw (compileToplevelModule filein) hscode

-- | Compile the given module to a runnable module.
compileToModule :: FilePath
                -> CompileConfig -> String -> (F.Module -> Compile [JsStmt]) -> String
                -> IO (Either CompileError (String,CompileState))
compileToModule filepath config raw with hscode = do
  result <- compileViaStr filepath config with hscode
  return $ case result of
    Left err -> Left err
    Right (PrintState{..},state,_) ->
      Right ( generateWrapped (concat $ reverse psOutput)
                              (stateModuleName state)
            , state
            )
  where
    generateWrapped jscode (ModuleName _ modulename) =
      unlines $ filter (not . null)
      [if configExportRuntime config then raw else ""
      ,jscode
      ,if not (configLibrary config)
          then unlines [";"
                       ,"Fay$$_(" ++ modulename ++ ".main);"
                       ]
          else ""
      ]

-- | Convert a Haskell filename to a JS filename.
toJsName :: String -> String
toJsName x = case reverse x of
  ('s':'h':'.': (reverse -> file)) -> file ++ ".js"
  _ -> x

-- | Print a compile error for human consumption.
showCompileError :: CompileError -> String
showCompileError e = case e of
  ParseError pos err -> err ++ " at line: " ++ show (srcLine pos) ++ " column: " ++ show (srcColumn pos)
  UnsupportedDeclaration d -> "unsupported declaration: " ++ prettyPrint d
  UnsupportedExportSpec es -> "unsupported export specification: " ++ prettyPrint es
  UnsupportedExpression expr -> "unsupported expression syntax: " ++ prettyPrint expr
  UnsupportedFieldPattern p -> "unsupported field pattern: " ++ prettyPrint p
  UnsupportedImport i -> "unsupported import syntax, we're too lazy: " ++ prettyPrint i
  UnsupportedLet -> "let not supported here"
  UnsupportedLetBinding d -> "unsupported let binding: " ++ prettyPrint d
  UnsupportedLiteral lit -> "unsupported literal syntax: " ++ prettyPrint lit
  UnsupportedModuleSyntax s m -> "unsupported module syntax in " ++ s ++ ": " ++ prettyPrint m
  UnsupportedPattern pat -> "unsupported pattern syntax: " ++ prettyPrint pat
  UnsupportedQualStmt stmt -> "unsupported list qualifier: " ++ prettyPrint stmt
  UnsupportedRecursiveDo -> "recursive `do' isn't supported"
  UnsupportedRhs rhs -> "unsupported right-hand side syntax: " ++ prettyPrint rhs
  UnsupportedWhereInAlt alt -> "`where' not supported here: " ++ prettyPrint alt
  UnsupportedWhereInMatch m -> "unsupported `where' syntax: " ++ prettyPrint m
  EmptyDoBlock -> "empty `do' block"
  InvalidDoBlock -> "invalid `do' block"
  FfiNeedsTypeSig d -> "your FFI declaration needs a type signature: " ++ prettyPrint d
  FfiFormatBadChars      srcloc cs -> printSrcSpanInfo srcloc ++ ": invalid characters for FFI format string: " ++ show cs
  FfiFormatNoSuchArg     srcloc i  -> printSrcSpanInfo srcloc ++ ": no such argument in FFI format string: " ++ show i
  FfiFormatIncompleteArg srcloc    -> printSrcSpanInfo srcloc ++ ": incomplete `%' syntax in FFI format string"
  FfiFormatInvalidJavaScript srcloc code err ->
    printSrcSpanInfo srcloc ++ ":" ++
    "\ninvalid JavaScript code in FFI format string:\n"
                                         ++ err ++ "\nin " ++ code
  Couldn'tFindImport i places ->
    "could not find an import in the path: " ++ prettyPrint i ++ ", \n" ++
    "searched in these places: " ++ intercalate ", " places
  UnableResolveQualified qname -> "unable to resolve qualified names " ++ prettyPrint qname
  GHCError s -> "ghc: " ++ s
--  FayScopeError s -> "scope error: " ++ s

-- | Get the JS runtime source.
getRuntime :: IO String
getRuntime = getDataFileName "js/runtime.js"
