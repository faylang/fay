{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
-- | Main compiler executable.

module Main where

import           Language.Fay
import           Language.Fay.Compiler
import           Language.Fay.Types

import           Control.Arrow
import qualified Control.Exception     as E
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.IO
import           Data.Default
import           Data.List
import           Data.Maybe
import           GHC.IO.Exception      (userError)
import           Options
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

-- / Options and help

defineOptions "FayCompilerOptions" $ do

  -- boolOption "optExportBuiltins" "export-builtins" True ""
  -- boolOption "optTCO" "tco" False ""

  boolOption     "optAutoRun"     "autorun"      False "automatically call main in generated JavaScript"
  boolOption     "optInlineForce" "inline-force" False "inline forcing, adds some speed for numbers, blows up code a bit"
  boolOption     "optFlattenApps" "flatten-apps" False "flatten function applicaton"
  boolOption     "optHTMLWrapper" "html-wrapper" False "Create an html file that loads the javascript"
  stringsOption  "optInclude"     "include"      []    "dir1[, ..] additional directories for include"

  option         "optStdout" (\o -> o
                              { optionLongFlags = ["stdout"]
                              , optionShortFlags = ['s']
                              , optionDefault = "false"
                              , optionType = optionTypeBool
                              , optionDescription = "Output to stdout"
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


helpTxt = [ "fayc -- The fay compiler from (a proper subset of) Haskell to Javascript"
          , "USAGE"
          , "  fayc [OPTIONS] [- | <hs-file>...]"
          , "  fayc - takes input on stdin and prints to stdout. Runs through js-beautify if available"
          , "  fayc <hs-file>... processes each .hs file"
          ]

-- | Main entry point.
main :: IO ()
main = runCommandHelp (unlines helpTxt) $ \opts files -> do
  let config = def { configTCO = False --optTCO opts
                   , configInlineForce = optInlineForce opts
                   , configFlattenApps = optFlattenApps opts
                   , configExportBuiltins = True -- optExportBuiltins opts

                   , configDirectoryIncludes = optInclude opts
                   , configPrettyPrint = optPretty opts
                   , configAutorun = optAutoRun opts
                   , configHtmlWrapper =  optHTMLWrapper opts
                   }
  
  E.catch (incompatible htmlAndStdout opts "Html wrapping and stdout are incompatible")
          errorUsage

  case files of
       ["-"] -> do
               hGetContents stdin >>= printCompile config compileModule
       [] -> errorUsage $ userError "No files specified"
       otherwise -> forM_ files $ \file -> do
             if optStdout opts
               then compileReadWrite config file stdout
               else
                  compileFromTo config file $ outPutFile opts file


  where
      -- | "12,34,5" => ["12","34","5"]
      split :: Eq a => a -> [a] -> [[a]]
      split _ [] = []
      split a as = takeWhile (/= a) as : split a (drop 1 $ dropWhile (/= a) as)

      outPutFile :: FayCompilerOptions -> String -> FilePath
      outPutFile opts file = fromMaybe (toJsName file) $ optOutput opts
      
      errorUsage :: IOError -> IO a
      errorUsage e = do
          putStrLn $ "ERROR: \n  " ++ (show e)
          args <- getArgs
          usageMsg args $ unlines $ drop 1 helpTxt

runCommandHelp :: (MonadIO m, Options opts) => String -> (opts -> [String] -> m a) -> m a
runCommandHelp help io = do
	argv <- liftIO getArgs
	let parsed = parseOptions argv
	case parsedOptions parsed of
		Just opts -> io opts (parsedArguments parsed)
		Nothing -> liftIO $ usageMsg argv help

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
