{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Main library entry point.

module Fay
  (module Fay.Config
  ,CompileError (..)
  ,CompileState (..)
  ,compileFile
  ,compileFileWithState
  ,compileFileWithRes
  ,compileFromTo
  ,compileFromToAndGenerateHtml
  ,toJsName
  ,showCompileError
  ,getConfigRuntime
  ,getRuntime
  ) where

import           Fay.Compiler
import           Fay.Compiler.Misc                      (ioWarn, printSrcSpanInfo)
import           Fay.Compiler.Packages
import           Fay.Compiler.Prelude
import           Fay.Compiler.Typecheck
import           Fay.Config
import qualified Fay.Exts                               as F
import           Fay.Types

import           Data.Aeson                             (encode)
import qualified Data.ByteString.Lazy                   as L
import           Language.Haskell.Exts.Annotated        (prettyPrint)
import           Language.Haskell.Exts.Annotated.Syntax
import           Language.Haskell.Exts.SrcLoc
import           Paths_fay
import           SourceMap                              (generate)
import           SourceMap.Types
import           System.FilePath

-- | Compile the given file and write the output to the given path, or
-- if nothing given, stdout.
compileFromTo :: Config -> FilePath -> Maybe FilePath -> IO ()
compileFromTo cfg filein fileout =
  if configTypecheckOnly cfg
  then do
    cfg' <- resolvePackages cfg
    res <- typecheck cfg' filein
    either (error . showCompileError) (ioWarn cfg') res
  else do
    result <- maybe (compileFile cfg filein)
                      (compileFromToAndGenerateHtml cfg filein)
                      fileout
    case result of
      Right out -> maybe (putStrLn out) (`writeFile` out) fileout
      Left err -> error $ showCompileError err

-- | Compile the given file and write to the output, also generate any HTML.
compileFromToAndGenerateHtml :: Config -> FilePath -> FilePath -> IO (Either CompileError String)
compileFromToAndGenerateHtml config filein fileout = do
  result <- compileFileWithRes config { configFilePath = Just filein } filein
  case result of
    Right (out,res) -> do
      when (configHtmlWrapper config) $
        writeFile (replaceExtension fileout "html") $ unlines [
            "<!doctype html>"
          , "<html>"
          , "  <head>"
          ,"    <meta http-equiv='Content-Type' content='text/html; charset=utf-8'>"
          , unlines . map (("    "++) . makeScriptTagSrc) $ configHtmlJSLibs config
          , "    " ++ makeScriptTagSrc relativeJsPath
          , "  </head>"
          , "  <body>"
          , "  </body>"
          , "</html>"]

      case (configSourceMap config, resSourceMappings res) of
        (True, Just mappings) ->
          L.writeFile (replaceExtension fileout "map") $
            encode $
              generate SourceMapping
                { smFile       = fileout
                , smSourceRoot = Nothing
                , smMappings   = mappings
                }
        _ -> return ()

      return $ Right (if configSourceMap config then sourceMapHeader ++ out else out)
            where relativeJsPath = makeRelative (dropFileName fileout) fileout
                  makeScriptTagSrc :: FilePath -> String
                  makeScriptTagSrc s = "<script type=\"text/javascript\" src=\"" ++ s ++ "\"></script>"
                  sourceMapHeader = "//@ sourceMappingURL=" ++ replaceExtension fileout "map" ++ "\n"
    Left err -> return (Left err)

-- | Compile the given file.
compileFile :: Config -> FilePath -> IO (Either CompileError String)
compileFile config filein = fmap (\(src,_,_) -> src) <$> compileFileWithState config filein

-- | Compile a file returning the resulting internal state of the compiler.
-- This is only to be used by the test suite.
compileFileWithState :: Config -> FilePath -> IO (Either CompileError (String,Maybe [Mapping],CompileState))
compileFileWithState config filein = do
  runtime <- getConfigRuntime config
  hscode <- readFile filein
  raw <- readFile runtime
  config' <- resolvePackages config
  compileToModule filein config' raw (compileToplevelModule filein) hscode

-- | Compile a file returning additional generated metadata.
compileFileWithRes :: Config -> FilePath -> IO (Either CompileError (String,CompileRes))
compileFileWithRes config filein = do
  res <- compileFileWithState config filein
  return $ do
    (s,m,st) <- res
    return ( s
           , CompileRes
               { resImported = map (first F.moduleNameString) $ stateImported st
               , resSourceMappings = m
               })

data CompileRes = CompileRes { resImported :: [(String, FilePath)], resSourceMappings :: Maybe [Mapping] }
  deriving Show

-- | Compile the given module to a runnable module.
compileToModule :: FilePath
                -> Config -> String -> (F.Module -> Compile [JsStmt]) -> String
                -> IO (Either CompileError (String,Maybe [Mapping],CompileState))
compileToModule filepath config raw with hscode = do
  result <- compileViaStr filepath config printState with hscode
  return $ case result of
    Left err -> Left err
    Right (ps,state,_) ->
      Right ( generateWrapped (concat . reverse $ psOutput ps)
                              (stateModuleName state)
            , if null (psMappings ps) then Nothing else Just (psMappings ps)
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
    printState = defaultPrintState
      { psPretty = configPrettyPrint config
      , psLine = length (lines raw) + 3
      }

-- | Convert a Haskell filename to a JS filename.
toJsName :: String -> String
toJsName x = case reverse x of
  ('s':'h':'.': (reverse -> file)) -> file ++ ".js"
  _ -> x

-- | Print a compile error for human consumption.
showCompileError :: CompileError -> String
showCompileError e = case e of
  Couldn'tFindImport i places      ->
    "could not find an import in the path: " ++ prettyPrint i ++ ", \n" ++
    "searched in these places: " ++ intercalate ", " places
  EmptyDoBlock -> "empty `do' block"
  FfiFormatBadChars srcloc cs      -> printSrcSpanInfo srcloc ++ ": invalid characters for FFI format string: " ++ show cs
  FfiFormatIncompleteArg srcloc    -> printSrcSpanInfo srcloc ++ ": incomplete `%' syntax in FFI format string"
  FfiFormatInvalidJavaScript l c m ->
    printSrcSpanInfo l ++ ":" ++
    "\ninvalid JavaScript code in FFI format string:\n" ++ m ++ "\nin " ++ c
  FfiFormatNoSuchArg srcloc i      ->
    printSrcSpanInfo srcloc ++ ":" ++
    "\nno such argument in FFI format string: " ++ show i
  FfiNeedsTypeSig d                -> "your FFI declaration needs a type signature: " ++ prettyPrint d
  GHCError s                       -> "ghc: " ++ s
  InvalidDoBlock                   -> "invalid `do' block"
  ParseError pos err               ->
    err ++ " at line: " ++ show (srcLine pos) ++ " column:" ++
    "\n" ++ show (srcColumn pos)
  ShouldBeDesugared s              -> "Expected this to be desugared (this is a bug): " ++ s
  UnableResolveQualified qname     -> "unable to resolve qualified names (this might be a bug):" ++ prettyPrint qname
  UnsupportedDeclaration d         -> "unsupported declaration: " ++ prettyPrint d
  UnsupportedEnum{}                -> "only Int is allowed in enum expressions"
  UnsupportedExportSpec es         -> "unsupported export specification: " ++ prettyPrint es
  UnsupportedExpression expr       -> "unsupported expression syntax: " ++ prettyPrint expr
  UnsupportedFieldPattern p        -> "unsupported field pattern: " ++ prettyPrint p
  UnsupportedImport i              -> "unsupported import syntax: " ++ prettyPrint i
  UnsupportedLet                   -> "let not supported here"
  UnsupportedLetBinding d          -> "unsupported let binding: " ++ prettyPrint d
  UnsupportedLiteral lit           -> "unsupported literal syntax: " ++ prettyPrint lit
  UnsupportedModuleSyntax s m      -> "unsupported module syntax in " ++ s ++ ": " ++ prettyPrint m
  UnsupportedPattern pat           -> "unsupported pattern syntax: " ++ prettyPrint pat
  UnsupportedQualStmt stmt         -> "unsupported list qualifier: " ++ prettyPrint stmt
  UnsupportedRecursiveDo           -> "recursive `do' isn't supported"
  UnsupportedRhs rhs               -> "unsupported right-hand side syntax: " ++ prettyPrint rhs
  UnsupportedWhereInAlt alt        -> "`where' not supported here: " ++ prettyPrint alt
  UnsupportedWhereInMatch m        -> "unsupported `where' syntax: " ++ prettyPrint m

-- | Get the JS runtime source.
-- This will return the user supplied runtime if it exists.
getConfigRuntime :: Config -> IO String
getConfigRuntime cfg = maybe getRuntime return $ configRuntimePath cfg

-- | Get the default JS runtime source.
getRuntime :: IO String
getRuntime = getDataFileName "js/runtime.js"
