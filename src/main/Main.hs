-- | Main compiler executable.

module Main where

import           Fay
import           Paths_fay                 (version)

import           Control.Applicative       ((<|>), (<$>), (<*>))
import qualified Control.Exception         as E
import           Control.Monad
import           Control.Monad.Reader
import           Data.List.Split           (wordsBy)
import           Data.Maybe
import           Data.Monoid               ((<>))
import           Data.Version              (showVersion)
import           Options.Applicative       (Mod, OptionFields, Parser, argument, execParser, fullDesc, header, help,
                                            helper, info, long, many, metavar, option, optional, short, strOption,
                                            switch, value)
import           Options.Applicative.Types (ReadM (ReadM))

-- | Options and help.
data FayCompilerOptions = FayCompilerOptions
  { optBasePath           :: Maybe FilePath
  , optGClosure           :: Bool
  , optFlattenApps        :: Bool
  , optHTMLJSLibs         :: [String]
  , optHTMLWrapper        :: Bool
  , optInclude            :: [String]
  , optLibrary            :: Bool
  , optNoGHC              :: Bool
  , optNoOptimizeNewtypes :: Bool
  , optNoRTS              :: Bool
  , optNoStdlib           :: Bool
  , optOptimize           :: Bool
  , optOutput             :: Maybe String
  , optPackages           :: [String]
  , optPackageConf        :: Maybe String
  , optPrettyAll          :: Bool
  , optPretty             :: Bool
  , optPrettyOperators    :: Bool
  , optPrettyThunks       :: Bool
  , optPrintRuntime       :: Bool
  , optRuntimePath        :: Maybe FilePath
  , optSourceMap          :: Bool
  , optStdlibOnly         :: Bool
  , optStdout             :: Bool
  , optStrict             :: [String]
  , optTypecheckOnly      :: Bool
  , optVersion            :: Bool
  , optWall               :: Bool
  , optShowGhcCalls       :: Bool
  , optTypeScript         :: Bool
  , optFiles              :: [String]
  }

-- | Main entry point.
main :: IO ()
main = do
  config' <- defaultConfigWithSandbox
  opts    <- execParser parser
  let config = addConfigDirectoryIncludePaths ("." : optInclude opts) $
        addConfigPackages (optPackages opts) $ config'
          { configOptimize         = optOptimize opts
          , configFlattenApps      = optFlattenApps opts
          , configPrettyPrint      = optPretty opts || optPrettyAll opts
          , configLibrary          = optLibrary opts
          , configHtmlWrapper      = optHTMLWrapper opts
          , configHtmlJSLibs       = optHTMLJSLibs opts
          , configTypecheck        = not $ optNoGHC opts
          , configWall             = optWall opts
          , configGClosure         = optGClosure opts
          , configPackageConf      = optPackageConf opts <|> configPackageConf config'
          , configExportRuntime    = not (optNoRTS opts)
          , configExportStdlib     = not (optNoStdlib opts)
          , configExportStdlibOnly = optStdlibOnly opts
          , configBasePath         = optBasePath opts
          , configStrict           = optStrict opts
          , configTypecheckOnly    = optTypecheckOnly opts
          , configRuntimePath      = optRuntimePath opts
          , configSourceMap        = optSourceMap opts
          , configOptimizeNewtypes = not $ optNoOptimizeNewtypes opts
          , configPrettyThunks     = optPrettyThunks opts || optPrettyAll opts
          , configPrettyOperators  = optPrettyOperators opts || optPrettyAll opts
          , configShowGhcCalls     = optShowGhcCalls opts
          , configTypeScript       = optTypeScript opts
          }
  if optVersion opts
    then runCommandVersion
    else if optPrintRuntime opts
      then getConfigRuntime config >>= readFile >>= putStr
      else do
        void $ incompatible htmlAndStdout opts "Html wrapping and stdout are incompatible"
        case optFiles opts of
          []    -> putStrLn $ helpTxt ++ "\n  More information: fay --help"
          files -> forM_ files $ \file ->
            compileFromTo config file (if optStdout opts then Nothing else Just (outPutFile opts file))

  where
    parser = info (helper <*> options) (fullDesc <> header helpTxt)

    outPutFile :: FayCompilerOptions -> String -> FilePath
    outPutFile opts file = fromMaybe (if optTypeScript opts then toTsName file else toJsName file) $ optOutput opts

-- | All Fay's command-line options.
options :: Parser FayCompilerOptions
options = FayCompilerOptions
  <$> optional (strOption $ long "base-path" <> help "If fay can't find the sources of fay-base you can use this to provide the path. Use --base-path ~/example instead of --base-path=~/example to make sure ~ is expanded properly")
  <*> switch (long "closure" <> help "Provide help with Google Closure")
  <*> switch (long "flatten-apps" <> help "flatten function applicaton")
  <*> strsOption (long "html-js-lib" <> metavar "file1[, ..]"
      <> help "javascript files to add to <head> if using option html-wrapper")
  <*> switch (long "html-wrapper" <> help "Create an html file that loads the javascript")
  <*> strsOption (long "include" <> metavar "dir1[, ..]"
      <> help "additional directories for include")
  <*> switch (long "library" <> help "Don't automatically call main in generated JavaScript")
  <*> switch (long "no-ghc" <> help "Don't typecheck, specify when not working with files")
  <*> switch (long "no-optimized-newtypes" <> help "Remove optimizations for newtypes, treating them as normal data types")
  <*> switch (long "no-rts" <> short 'r' <> help "Don't export the RTS")
  <*> switch (long "no-stdlib" <> help "Don't generate code for the Prelude/FFI")
  <*> switch (long "optimize" <> short 'O' <> help "Apply optimizations to generated code")
  <*> optional (strOption (long "output" <> short 'o' <> metavar "file" <> help "Output to specified file"))
  <*> strsOption (long "package" <> metavar "package[, ..]"
      <> help "packages to use for compilation")
  <*> optional (strOption (long "package-conf" <> help "Specify the Cabal package config file"))
  <*> switch (long "pretty-all" <> help "Pretty print, pretty operators and pretty thunks")
  <*> switch (long "pretty" <> short 'p' <> help "Pretty print the output")
  <*> switch (long "pretty-operators" <> help "Use pretty operators")
  <*> switch (long "pretty-thunks" <> help "Use pretty thunk names")
  <*> switch (long "print-runtime" <> help "Print the runtime JS source to stdout")
  <*> optional (strOption $ long "runtime-path" <> help "Custom path to the runtime so you don't have to reinstall fay when modifying it")
  <*> switch (long "sourcemap" <> help "Produce a source map in <outfile>.map")
  <*> switch (long "stdlib" <> help "Only output the stdlib")
  <*> switch (long "stdout" <> short 's' <> help "Output to stdout")
  <*> strsOption (long "strict" <> metavar "modulename[, ..]"
      <> help "Generate strict and uncurried exports for the supplied modules. Simplifies calling Fay from JS")
  <*> switch (long "typecheck-only" <> help "Only invoke GHC for typechecking, don't produce any output")
  <*> switch (long "version" <> help "Output version number")
  <*> switch (long "Wall" <> help "Typecheck with -Wall")
  <*> switch (long "show-ghc-calls" <> help "Print commands sent to ghc")
  <*> switch (long "ts" <> help "Output TypeScript instead of JavaScript")
  <*> many (argument (ReadM ask) (metavar "<hs-file>..."))
  where
    strsOption :: Mod OptionFields [String] -> Parser [String]
    strsOption m = option (ReadM . fmap (wordsBy (== ',')) $ ask) (m <> value [])

-- | Make incompatible options.
incompatible :: Monad m
  => (FayCompilerOptions -> Bool)
  -> FayCompilerOptions -> String -> m Bool
incompatible test opts message = if test opts
  then E.throw $ userError message
  else return True

-- | The basic help text.
helpTxt :: String
helpTxt
  =  "fay -- The fay compiler from (a proper subset of) Haskell to Javascript\n\n"
  ++ "  fay <hs-file>... processes each .hs file"

-- | Print the command version.
runCommandVersion :: IO ()
runCommandVersion = putStrLn $ "fay " ++ showVersion version

-- | Incompatible options.
htmlAndStdout :: FayCompilerOptions -> Bool
htmlAndStdout opts = optHTMLWrapper opts && optStdout opts
