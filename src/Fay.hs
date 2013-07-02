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
import           Fay.Compiler.Misc   (printSrcLoc)
import           Fay.Compiler.Packages
import           Fay.Compiler.Print
import           Fay.Types

import           Control.Applicative
import           Control.Monad
import           Data.List
import qualified Data.Set                     as S
import           Language.Haskell.Exts        (prettyPrint)
import           Language.Haskell.Exts.Syntax
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
  either Left (Right . fst) <$> compileFileWithState config filein

-- | Compile a file returning the state.
compileFileWithState :: CompileConfig -> FilePath -> IO (Either CompileError (String,CompileState))
compileFileWithState config filein = do
  runtime <- getRuntime
  hscode <- readFile filein
  raw <- readFile runtime
  config' <- resolvePackages config
  compileToModule filein config' raw compileToplevelModule hscode

-- | Compile the given module to a runnable module.
compileToModule :: (Show from,Show to,CompilesTo from to)
                => FilePath
                -> CompileConfig -> String -> (from -> Compile to) -> String
                -> IO (Either CompileError (String,CompileState))
compileToModule filepath config raw with hscode = do
  result <- compileViaStr filepath config with hscode
  case result of
    Left err -> return (Left err)
    Right (PrintState{..},state,_) ->
      return $ Right $ (generate (concat (reverse psOutput))
                                        (S.toList $ getCurrentExportsWithoutNewtypes state)
                                        (stateModuleName state), state)

  where generate | configNaked config = generateNaked
                 | otherwise          = generateWrapped
        generateNaked jscode _exports _module = unlines $
          [if configExportRuntime config then raw else ""
          ,jscode]
        generateWrapped jscode exports (ModuleName (clean -> modulename)) = unlines $ filter (not . null) $
          ["/** @constructor"
          ,"*/"
          ,"var " ++ modulename ++ " = function(){"
          ,if configExportRuntime config then raw else ""
          ,jscode
          ,"// Exports"
          ,unlines (map printExport exports)
          ,"// Built-ins"
          ,"this._ = Fay$$_;"
          ,if configExportBuiltins config
              then unlines ["this.$           = Fay$$$;"
                           ,"this.$fayToJs    = Fay$$fayToJs;"
                           ,"this.$jsToFay    = Fay$$jsToFay;"
                           ]
              else ""
          ,"};"
          ,if not (configLibrary config)
              then unlines [";"
                           ,"var main = new " ++ modulename ++ "();"
                           ,"main._(main." ++ modulename ++ "$main);"
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
showCompileError e = case e of
  ParseError pos err -> err ++ " at line: " ++ show (srcLine pos) ++ " column: " ++ show (srcColumn pos)
  UnsupportedDeclaration d -> "unsupported declaration: " ++ prettyPrint d
  UnsupportedExportSpec es -> "unsupported export specification: " ++ prettyPrint es
  UnsupportedMatchSyntax m -> "unsupported match/binding syntax: " ++ prettyPrint m
  UnsupportedWhereInMatch m -> "unsupported `where' syntax: " ++ prettyPrint m
  UnsupportedWhereInAlt alt -> "`where' not supported here: " ++ prettyPrint alt
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
  FfiFormatBadChars      srcloc cs -> printSrcLoc srcloc ++ ": invalid characters for FFI format string: " ++ show cs
  FfiFormatNoSuchArg     srcloc i  -> printSrcLoc srcloc ++ ": no such argument in FFI format string: " ++ show i
  FfiFormatIncompleteArg srcloc    -> printSrcLoc srcloc ++ ": incomplete `%' syntax in FFI format string"
  FfiFormatInvalidJavaScript srcloc code err ->
    printSrcLoc srcloc ++ ":" ++
    "\ninvalid JavaScript code in FFI format string:\n"
                                         ++ err ++ "\nin " ++ code
  UnsupportedFieldPattern p -> "unsupported field pattern: " ++ prettyPrint p
  UnsupportedImport i -> "unsupported import syntax, we're too lazy: " ++ prettyPrint i
  Couldn'tFindImport i places ->
    "could not find an import in the path: " ++ prettyPrint i ++ ", \n" ++
    "searched in these places: " ++ intercalate ", " places
  UnableResolveUnqualified name -> "unable to resolve unqualified name " ++ prettyPrint name
  UnableResolveQualified qname -> "unable to resolve qualified names " ++ prettyPrint qname
  GHCError s -> "ghc: " ++ s

-- | Get the JS runtime source.
getRuntime :: IO String
getRuntime = getDataFileName "js/runtime.js"
