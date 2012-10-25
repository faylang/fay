{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
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
import           Data.List.Split          (wordsBy)
import           Data.Maybe
import           Data.Version             (showVersion)
import           Options.Applicative
import           System.Console.Haskeline
import           System.IO

-- | Options and help.
data FayCompilerOptions = FayCompilerOptions
    { optLibrary     :: Bool
    , optFlattenApps :: Bool
    , optHTMLWrapper :: Bool
    , optHTMLJSLibs  :: [String]
    , optInclude     :: [String]
    , optWall        :: Bool
    , optNoGHC       :: Bool
    , optStdout      :: Bool
    , optVersion     :: Bool
    , optOutput      :: Maybe String
    , optPretty      :: Bool
    , optFiles       :: [String]
    }

options :: Parser FayCompilerOptions
options = FayCompilerOptions
  <$> switch (long "library" & help "Don't automatically call main in generated JavaScript")
  <*> switch (long "flatten-apps" & help "flatten function applicaton")

  <*> switch (long "html-wrapper" & help "Create an html file that loads the javascript")
  <*> strsOption (long "html-js-lib" & metavar "file1[, ..]" & help "javascript files to add to <head> if using option html-wrapper")

  <*> strsOption (long "include" & metavar "dir1[, ..]" & help "additional directories for include")

  <*> switch (long "Wall" & help "Typecheck with -Wall")
  <*> switch (long "no-ghc" & help "Don't typecheck, specify when not working with files")

  <*> switch (long "stdout" & short 's' & help "Output to stdout")
  <*> switch (long "version" & help "Output version number")
  <*> nullOption (long "output" & short 'o' & reader (Just . Just) & value Nothing & help "Output to specified file")
  <*> switch (long "pretty" & short 'p' & help "Pretty print the output")

  <*> arguments Just (metavar "- | <hs-file>...")

  where
    strsOption m = nullOption (m & reader (Just . wordsBy (== ',')) & value [])

-- | The basic help text.
helpTxt :: String
helpTxt = concat
  ["fay -- The fay compiler from (a proper subset of) Haskell to Javascript\n\n"
  ,"SYNOPSIS\n"
  ,"  fay [OPTIONS] [- | <hs-file>...]\n"
  ,"  fay - takes input on stdin and prints to stdout. Pretty prints\n"
  ,"  fay <hs-file>... processes each .hs file"
  ]

-- | Main entry point.
main :: IO ()
main = do
  opts <- execParser parser
  if optVersion opts
    then runCommandVersion
    else (do
  let config = def { configTCO = False -- optTCO opts
                   , configFlattenApps = optFlattenApps opts
                   , configExportBuiltins = True -- optExportBuiltins opts
                   , configDirectoryIncludes = "." : optInclude opts
                   , configPrettyPrint = optPretty opts
                   , configLibrary = optLibrary opts
                   , configHtmlWrapper =  optHTMLWrapper opts
                   , configHtmlJSLibs = optHTMLJSLibs opts
                   , configTypecheck = not $ optNoGHC opts
                   , configWall = optWall opts
                   }
  void $ incompatible htmlAndStdout opts "Html wrapping and stdout are incompatible"

  case optFiles opts of
       ["-"] -> do
               hGetContents stdin >>= printCompile config compileModule
       [] -> runInteractive
       files  -> forM_ files $ \file -> do
               if optStdout opts
                 then compileReadWrite config file stdout
                 else
                    compileFromTo config file $ outPutFile opts file)


  where
    parser = info (helper <*> options) (fullDesc & header helpTxt)

    outPutFile :: FayCompilerOptions -> String -> FilePath
    outPutFile opts file = fromMaybe (toJsName file) $ optOutput opts

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
                    Left err -> do
                      -- an error occured, maybe input was not an expression,
                      -- but a declaration, try compiling the input as a declaration
                      outputStrLn ("can't parse input as expression: " ++ show err)
                      result' <- liftIO $ compileViaStr def (compileDecl True) input
                      case result' of
                        Right (ok,_) -> liftIO (prettyPrintString ok) >>= outputStr
                        Left err' ->
                          outputStrLn ("can't parse input as declaration: " ++ show err')
                    Right (ok,_) -> liftIO (prettyPrintString ok) >>= outputStr
                loop

runCommandVersion :: IO ()
runCommandVersion = putStrLn $ "fay " ++ showVersion version



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
