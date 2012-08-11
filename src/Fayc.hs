{-# LANGUAGE TemplateHaskell #-}
-- | Main compiler executable.

module Main where

import Language.Fay (compileModule, compileViaStr)
import Language.Fay.Compiler
import Language.Fay.Types

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.List
import Options
import System.Environment
import System.Exit
import System.IO
import System.Process

defineOptions "FayCompilerOptions" $ do
  boolOption "optTCO" "tco" False ""
  boolOption "optAutoRun" "autorun" False ""
  boolOption "optInlineForce" "inline-force" False ""
  boolOption "optFlattenApps" "flatten-apps" False ""
  boolOption "optExportBuiltins" "export-builtins" True ""
  option "optStdout" (\o -> o
        { optionLongFlags = ["stdout"]
        , optionShortFlags = ['s']
        , optionDefault = "false"
        , optionType = optionTypeBool
        , optionDescription = "Output to stdout"
                            })
  option "optPretty" (\o -> o
        { optionLongFlags = ["pretty"]
        , optionShortFlags = ['p']
        , optionDefault = "false"
        , optionType = optionTypeBool
        , optionDescription = "Run javascript through js-beautify"
                            })
        
helpTxt :: String
helpTxt = unlines [
          "fayc -- The fay compiler from (a proper subset of) Haskell to Javascript"
        , "USAGE"
        , "  fayc [OPTIONS] [- | <hs-file>...]"
        , "  fayc - takes input on stdin and pretty prints to stdout. Requires js-beautify"
        , "  fayc <hs-file>... processes each .hs file"
        ]

main :: IO ()
main = runCommandHelp helpTxt $ \opts files -> do
    let config = def { configTCO = optTCO opts
                     , configInlineForce = optInlineForce opts
                     , configFlattenApps = optFlattenApps opts
                     , configExportBuiltins = optExportBuiltins opts
                     }
    let compile' = compile config (optAutoRun opts)
    case files of
         ["-"] -> do
               putStrLn =<< prettyCompile config compileModule =<< hGetContents stdin
                     
         otherwise -> forM_ files $ \file -> do
             compile'
                     (readFile file)
                     (writer opts file)
    where
        --format :: (Options opts) => opts -> (String -> IO String)
        format opts = case optPretty opts of
                       True -> beautify
                       False -> return . id
        writer opts file js = do
                        js' <- format opts js
                        case optStdout opts of
                            True -> hPutStr stdout js'
                            False -> writeFile (toJsName file) js'
                  
               
runCommandHelp :: (MonadIO m, Options opts) => String -> (opts -> [String] -> m a) -> m a
runCommandHelp help io = do
	argv <- liftIO System.Environment.getArgs
	let parsed = parseOptions argv
	case parsedOptions parsed of
		Just opts -> io opts (parsedArguments parsed)
		Nothing -> liftIO $ do
                        putStrLn help
                        case parsedError parsed of
			    Just err -> do
			    	hPutStrLn stderr (parsedHelp parsed)
			    	hPutStrLn stderr err
			    	exitFailure
			    Nothing -> do
			    	hPutStr stdout (parsedHelp parsed)
			    	exitSuccess

prettyCompile :: (Show from,Show to,CompilesTo from to)
              => CompileConfig
              -> (from -> Compile to)
              -> String
              -> IO (String)
prettyCompile config with from = do
  result <- compileViaStr config with from
  case result of
    Left err -> return $ show err
    Right (ok,_) -> beautify ok
                      
beautify :: String -> IO (String)
beautify js = do
    (Just hin, Just hout, _, _) <-
       createProcess (proc "js-beautify" ["-"]){ std_in = CreatePipe, std_out = CreatePipe }
    hPutStrLn hin js
    hClose hin
    hGetContents hout