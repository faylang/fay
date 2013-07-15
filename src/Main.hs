{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# OPTIONS -fno-warn-orphans #-}
-- | Main compiler executable.

module Main where

import           Fay
import           Fay.Compiler
import           Fay.Compiler.Config
import           Fay.Compiler.Debug

import qualified Control.Exception        as E
import           Control.Monad
import           Control.Monad.Error
import           Data.Default
import           Data.List.Split          (wordsBy)
import           Data.Maybe
import           Data.Version             (showVersion)
import           Options.Applicative
import           Paths_fay                (version)
import           System.Console.Haskeline
import           System.Environment
import           System.IO

-- | Options and help.
data FayCompilerOptions = FayCompilerOptions
  { optLibrary      :: Bool
  , optFlattenApps  :: Bool
  , optHTMLWrapper  :: Bool
  , optHTMLJSLibs   :: [String]
  , optInclude      :: [String]
  , optPackages     :: [String]
  , optWall         :: Bool
  , optNoGHC        :: Bool
  , optStdout       :: Bool
  , optVersion      :: Bool
  , optOutput       :: Maybe String
  , optPretty       :: Bool
  , optFiles        :: [String]
  , optOptimize     :: Bool
  , optGClosure     :: Bool
  , optPackageConf  :: Maybe String
  , optNoRTS        :: Bool
  , optNoStdlib     :: Bool
  , optPrintRuntime :: Bool
  , optStdlibOnly   :: Bool
  , optNoBuiltins   :: Bool
  , optBasePath     :: Maybe FilePath
  }

-- | Main entry point.
main :: IO ()
main = do
  packageConf <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  opts <- execParser parser
  if optVersion opts
    then runCommandVersion
    else do
      if optPrintRuntime opts
         then getRuntime >>= readFile >>= putStr
         else do
           let config = addConfigDirectoryIncludePaths ("." : optInclude opts) $
                 addConfigPackages (optPackages opts) $ def
                   { configOptimize         = optOptimize opts
                   , configFlattenApps      = optFlattenApps opts
                   , configExportBuiltins   = not (optNoBuiltins opts)
                   , configPrettyPrint      = optPretty opts
                   , configLibrary          = optLibrary opts
                   , configHtmlWrapper      = optHTMLWrapper opts
                   , configHtmlJSLibs       = optHTMLJSLibs opts
                   , configTypecheck        = not $ optNoGHC opts
                   , configWall             = optWall opts
                   , configGClosure         = optGClosure opts
                   , configPackageConf      = optPackageConf opts <|> packageConf
                   , configExportRuntime    = not (optNoRTS opts)
                   , configExportStdlib     = not (optNoStdlib opts)
                   , configExportStdlibOnly = optStdlibOnly opts
                   , configBasePath         = optBasePath opts
                   }
           void $ incompatible htmlAndStdout opts "Html wrapping and stdout are incompatible"
           case optFiles opts of
                ["-"] -> hGetContents stdin >>= printCompile config compileModuleFromAST
                []    -> runInteractive
                files -> forM_ files $ \file -> do
                  if optStdout opts
                    then compileFromTo config file Nothing
                    else compileFromTo config file (Just (outPutFile opts file))

  where
    parser = info (helper <*> options) (fullDesc <> header helpTxt)

    outPutFile :: FayCompilerOptions -> String -> FilePath
    outPutFile opts file = fromMaybe (toJsName file) $ optOutput opts

-- | All Fay's command-line options.
options :: Parser FayCompilerOptions
options = FayCompilerOptions
  <$> switch (long "library" <> help "Don't automatically call main in generated JavaScript")
  <*> switch (long "flatten-apps" <> help "flatten function applicaton")
  <*> switch (long "html-wrapper" <> help "Create an html file that loads the javascript")
  <*> strsOption (long "html-js-lib" <> metavar "file1[, ..]"
      <> help "javascript files to add to <head> if using option html-wrapper")
  <*> strsOption (long "include" <> metavar "dir1[, ..]"
      <> help "additional directories for include")
  <*> strsOption (long "package" <> metavar "package[, ..]"
      <> help "packages to use for compilation")
  <*> switch (long "Wall" <> help "Typecheck with -Wall")
  <*> switch (long "no-ghc" <> help "Don't typecheck, specify when not working with files")
  <*> switch (long "stdout" <> short 's' <> help "Output to stdout")
  <*> switch (long "version" <> help "Output version number")
  <*> optional (strOption (long "output" <> short 'o' <> metavar "file" <> help "Output to specified file"))
  <*> switch (long "pretty" <> short 'p' <> help "Pretty print the output")
  <*> arguments Just (metavar "- | <hs-file>...")
  <*> switch (long "optimize" <> short 'O' <> help "Apply optimizations to generated code")
  <*> switch (long "closure" <> help "Provide help with Google Closure")
  <*> optional (strOption (long "package-conf" <> help "Specify the Cabal package config file"))
  <*> switch (long "no-rts" <> short 'r' <> help "Don't export the RTS")
  <*> switch (long "no-stdlib" <> help "Don't generate code for the Prelude/FFI")
  <*> switch (long "print-runtime" <> help "Print the runtime JS source to stdout")
  <*> switch (long "stdlib" <> help "Only output the stdlib")
  <*> switch (long "no-builtins" <> help "Don't export no-builtins")
  <*> optional (strOption (long "base-path" <> help "If fay can't find the sources of fay-base you can use this to provide the path. Use --base-path ~/example instead of --base-path=~/example to make sure ~ is expanded properly"))

  where strsOption m =
          nullOption (m <> reader (Right . wordsBy (== ',')) <> value [])


-- | Make incompatible options.
incompatible :: Monad m
  => (FayCompilerOptions -> Bool)
  -> FayCompilerOptions -> String -> m Bool
incompatible test opts message = case test opts of
  True -> E.throw $ userError message
  False -> return True

-- | The basic help text.
helpTxt :: String
helpTxt = concat
  ["fay -- The fay compiler from (a proper subset of) Haskell to Javascript\n\n"
  ,"SYNOPSIS\n"
  ,"  fay [OPTIONS] [- | <hs-file>...]\n"
  ,"  fay - takes input on stdin and prints to stdout. Pretty prints\n"
  ,"  fay <hs-file>... processes each .hs file"
  ]

-- | Print the command version.
runCommandVersion :: IO ()
runCommandVersion = putStrLn $ "fay " ++ showVersion version

-- | Incompatible options.
htmlAndStdout :: FayCompilerOptions -> Bool
htmlAndStdout opts = optHTMLWrapper opts && optStdout opts

-- | Run interactively.
runInteractive :: IO ()
runInteractive = runInputT defaultSettings loop where
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> return ()
      Just "" -> loop
      Just input -> do
        result <- liftIO $ compileViaStr "<interactive>" config compileExp input
        case result of
          Left err -> do
            -- an error occured, maybe input was not an expression,
            -- but a declaration, try compiling the input as a declaration
            outputStrLn ("can't parse input as expression: " ++ show err)
            result' <- liftIO $ compileViaStr "<interactive>" config (compileDecl True) input
            case result' of
              Right (PrintState{..},_,_) -> outputStr (concat (reverse psOutput))
              Left err' ->
                outputStrLn ("can't parse input as declaration: " ++ show err')
          Right (PrintState{..},_,_) -> outputStr (concat (reverse psOutput))
        loop
  config = def { configPrettyPrint = True }
