{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
-- | Main compiler executable.

module Main where

import           Language.Fay
import           Language.Fay.Compiler
import           Language.Fay.Types
import           Paths_fay                (version)

import qualified Control.Exception        as E
import           Control.Monad
import           Control.Monad.Error
import           Data.Default
import           Data.Maybe
import           Data.Version             (showVersion)
import           Options
import           System.Console.Haskeline
import           System.Environment
import           System.Exit
import           System.IO

-- | Options and help.
defineOptions "FayCompilerOptions" $ do

  -- boolOption "optExportBuiltins" "export-builtins" True ""
  -- boolOption "optTCO" "tco" False ""

  boolOption     "optAutoRun"     "autorun"      False "automatically call main in generated JavaScript"
  boolOption     "optInlineForce" "inline-force" False "inline forcing, adds some speed for numbers, blows up code a bit"
  boolOption     "optFlattenApps" "flatten-apps" False "flatten function applicaton"

  boolOption     "optHTMLWrapper" "html-wrapper" False "Create an html file that loads the javascript"
  stringsOption  "optHTMLJSLibs"  "html-js-lib"  []    "file1[, ..] javascript files to add to <head> if using option html-wrapper"

  stringsOption  "optInclude"     "include"      []    "dir1[, ..] additional directories for include"

  boolOption     "optWall"        "Wall"         False "Typecheck with -Wall"
  boolOption     "optNoGHC"       "no-ghc"       False "Don't typecheck, specify when not working with files"

  option         "optStdout" (\o -> o
                              { optionLongFlags = ["stdout"]
                              , optionShortFlags = ['s']
                              , optionDefault = "false"
                              , optionType = optionTypeBool
                              , optionDescription = "Output to stdout"
                                                  })
  option         "optVersion" (\o -> o
                              { optionLongFlags = ["version"]
                              , optionShortFlags = ['v']
                              , optionDefault = "false"
                              , optionType = optionTypeBool
                              , optionDescription = "Output version number"
                                                  })
  option         "optOutput" (\o -> o
                              { optionLongFlags = ["output"]
                              , optionShortFlags = ['o']
                              , optionDefault = ""
                              , optionType = optionTypeMaybe optionTypeString
                              , optionDescription = "Output to specified file"
                                                  })
  option         "optPretty" (\o -> o
                             { optionLongFlags = ["pretty"]
                             , optionShortFlags = ['p']
                             , optionDefault = "false"
                             , optionType = optionTypeBool
                             , optionDescription = "Run javascript through js-beautify"
                                                 })

-- | The basic help text.
helpTxt :: [String]
helpTxt =
  ["fay -- The fay compiler from (a proper subset of) Haskell to Javascript"
  ,"USAGE"
  ,"  fay [OPTIONS] [- | <hs-file>...]"
  ,"  fay - takes input on stdin and prints to stdout. Runs through js-beautify if available"
  ,"  fay <hs-file>... processes each .hs file"
  ]

-- | Main entry point.
main :: IO ()
main =
  runCommandHelp (unlines helpTxt) $ \opts files ->
    if optVersion opts
      then runCommandVersion
      else (do
  let config = def { configTCO = False -- optTCO opts
                   , configInlineForce = optInlineForce opts
                   , configFlattenApps = optFlattenApps opts
                   , configExportBuiltins = True -- optExportBuiltins opts

                   , configDirectoryIncludes = "." : optInclude opts
                   , configPrettyPrint = optPretty opts
                   , configAutorun = optAutoRun opts
                   , configHtmlWrapper =  optHTMLWrapper opts
                   , configHtmlJSLibs = optHTMLJSLibs opts
                   , configTypecheck = not $ optNoGHC opts
                   , configWall = optWall opts
                   }
  void $ E.catch (incompatible htmlAndStdout opts "Html wrapping and stdout are incompatible")
                 errorUsage

  case files of
       ["-"] -> do
               hGetContents stdin >>= printCompile config compileModule
       [] -> runInteractive
       _  -> forM_ files $ \file -> do
               if optStdout opts
                 then compileReadWrite config file stdout
                 else
                    compileFromTo config file $ outPutFile opts file)


  where
    outPutFile :: FayCompilerOptions -> String -> FilePath
    outPutFile opts file = fromMaybe (toJsName file) $ optOutput opts

    errorUsage :: IOError -> IO a
    errorUsage e = do
        putStrLn $ "ERROR: \n  " ++ (show e)
        args <- getArgs
        usageMsg args $ unlines $ drop 1 helpTxt

runInteractive :: IO ()
runInteractive =
    runInputT defaultSettings loop
  where
    loop = do
        minput <- getInputLine "> "
        case minput of
            Nothing -> return ()
            Just "" -> loop
            Just input -> do
                result <- liftIO $ compileViaStr def compileExp input
                case result of
                    Left err -> outputStrLn . show $ err
                    Right (ok,_) -> liftIO (prettyPrintString ok) >>= outputStr
                loop

runCommandHelp :: (MonadIO m, Options opts) => String -> (opts -> [String] -> m a) -> m a
runCommandHelp help io = do
	argv <- liftIO getArgs
	let parsed = parseOptions argv
	case parsedOptions parsed of
		Just opts -> io opts (parsedArguments parsed)
		Nothing -> liftIO $ usageMsg argv help

runCommandVersion :: IO ()
runCommandVersion = putStrLn $ "fay " ++ showVersion version



usageMsg :: [String] -> String -> IO a
usageMsg argv help = do
    putStrLn help
    let parsed = parseOptions argv :: ParsedOptions FayCompilerOptions
    case parsedError parsed of
        Just err -> do
            hPutStrLn stderr (parsedHelp parsed)
            hPutStrLn stderr err
            exitFailure
	Nothing -> do
	    hPutStr stdout (parsedHelp parsed)
	    exitSuccess

htmlAndStdout :: FayCompilerOptions -> Bool
htmlAndStdout opts = optHTMLWrapper opts && optStdout opts

incompatible :: Monad m => (FayCompilerOptions -> Bool)
                        -> FayCompilerOptions -> String -> m Bool
incompatible test opts message = case test opts of
             True -> E.throw $ userError message
             False -> return True

instance Writer Handle where
  writeout = hPutStr

instance Reader Handle where
  readin = hGetContents
