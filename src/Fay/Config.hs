-- | Configuring the compiler

module Fay.Config
  ( Config
      ( configOptimize
      , configFlattenApps
      , configExportRuntime
      , configExportStdlib
      , configExportStdlibOnly
      , configPrettyPrint
      , configHtmlWrapper
      , configSourceMap
      , configHtmlJSLibs
      , configLibrary
      , configWarn
      , configFilePath
      , configTypecheck
      , configWall
      , configGClosure
      , configPackageConf
      , configBasePath
      , configStrict
      , configTypecheckOnly
      , configRuntimePath
      , configOptimizeNewtypes
      , configPrettyThunks
      , configPrettyOperators
      )
  , defaultConfig
  , defaultConfigWithSandbox
  , configDirectoryIncludes
  , configDirectoryIncludePaths
  , nonPackageConfigDirectoryIncludePaths
  , addConfigDirectoryInclude
  , addConfigDirectoryIncludes
  , addConfigDirectoryIncludePaths
  , configPackages
  , addConfigPackage
  , addConfigPackages
  , shouldExportStrictWrapper
  ) where

import           Fay.Compiler.Prelude

import           Data.Default
import           Data.Maybe                      ()
import           Language.Haskell.Exts.Annotated (ModuleName (..))
import           System.Environment

-- | Configuration of the compiler.
-- The fields with a leading underscore
data Config = Config
  { configOptimize           :: Bool                        -- ^ Run optimizations
  , configFlattenApps        :: Bool                        -- ^ Flatten function application?
  , configExportRuntime      :: Bool                        -- ^ Export the runtime?
  , configExportStdlib       :: Bool                        -- ^ Export the stdlib?
  , configExportStdlibOnly   :: Bool                        -- ^ Export /only/ the stdlib?
  , _configDirectoryIncludes :: [(Maybe String, FilePath)]  -- ^ Possibly a fay package name, and a include directory.
  , configPrettyPrint        :: Bool                        -- ^ Pretty print the JS output?
  , configHtmlWrapper        :: Bool                        -- ^ Output a HTML file including the produced JS.
  , configSourceMap          :: Bool                        -- ^ Output a source map file as outfile.map.
  , configHtmlJSLibs         :: [FilePath]                  -- ^ Any JS files to link to in the HTML.
  , configLibrary            :: Bool                        -- ^ Don't invoke main in the produced JS.
  , configWarn               :: Bool                        -- ^ Warn on dubious stuff, not related to typechecking.
  , configFilePath           :: Maybe FilePath              -- ^ File path to output to.
  , configTypecheck          :: Bool                        -- ^ Typecheck with GHC.
  , configWall               :: Bool                        -- ^ Typecheck with -Wall.
  , configGClosure           :: Bool                        -- ^ Run Google Closure on the produced JS.
  , configPackageConf        :: Maybe FilePath              -- ^ The package config e.g. packages-6.12.3.
  , _configPackages          :: [String]                    -- ^ Included Fay packages.
  , configBasePath           :: Maybe FilePath              -- ^ Custom source location for fay-base
  , configStrict             :: [String]                    -- ^ Produce strict and uncurried JavaScript callable wrappers for all
                                                            --   exported functions with type signatures in the given module
  , configTypecheckOnly      :: Bool                        -- ^ Only invoke GHC for typechecking, don't produce any output
  , configRuntimePath        :: Maybe FilePath
  , configOptimizeNewtypes   :: Bool                        -- ^ Optimize away newtype constructors?
  , configPrettyThunks       :: Bool                        -- ^ Use pretty thunk names?
  , configPrettyOperators    :: Bool                        -- ^ Use pretty operators?
  } deriving (Show)

defaultConfig :: Config
defaultConfig = addConfigPackage "fay-base"
  Config
    { configOptimize           = False
    , configFlattenApps        = False
    , configExportRuntime      = True
    , configExportStdlib       = True
    , configExportStdlibOnly   = False
    , _configDirectoryIncludes = []
    , configPrettyPrint        = False
    , configHtmlWrapper        = False
    , configHtmlJSLibs         = []
    , configLibrary            = False
    , configWarn               = True
    , configFilePath           = Nothing
    , configTypecheck          = True
    , configWall               = False
    , configGClosure           = False
    , configPackageConf        = Nothing
    , _configPackages          = []
    , configBasePath           = Nothing
    , configStrict             = []
    , configTypecheckOnly      = False
    , configRuntimePath        = Nothing
    , configSourceMap          = False
    , configOptimizeNewtypes   = True
    , configPrettyThunks       = False
    , configPrettyOperators    = False
    }

defaultConfigWithSandbox :: IO Config
defaultConfigWithSandbox = do
  packageConf <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
  return defaultConfig { configPackageConf = packageConf }

-- | Default configuration.
instance Default Config where
  def = defaultConfig

-- | Reading _configDirectoryIncludes is safe to do.
configDirectoryIncludes :: Config -> [(Maybe String, FilePath)]
configDirectoryIncludes = _configDirectoryIncludes

-- | Get all include directories without the package mapping.
configDirectoryIncludePaths :: Config -> [FilePath]
configDirectoryIncludePaths = map snd . _configDirectoryIncludes

-- | Get all include directories not included through packages.
nonPackageConfigDirectoryIncludePaths :: Config -> [FilePath]
nonPackageConfigDirectoryIncludePaths = map snd . filter (isJust . fst) . _configDirectoryIncludes

-- | Add a mapping from (maybe) a package to a source directory
addConfigDirectoryInclude :: Maybe String -> FilePath -> Config -> Config
addConfigDirectoryInclude pkg fp cfg = cfg { _configDirectoryIncludes = (pkg, fp) : _configDirectoryIncludes cfg }

-- | Add several include directories.
addConfigDirectoryIncludes :: [(Maybe String,FilePath)] -> Config -> Config
addConfigDirectoryIncludes pkgFps cfg = foldl (\c (pkg,fp) -> addConfigDirectoryInclude pkg fp c) cfg pkgFps

-- | Add several include directories without package references.
addConfigDirectoryIncludePaths :: [FilePath] -> Config -> Config
addConfigDirectoryIncludePaths fps cfg = foldl (flip (addConfigDirectoryInclude Nothing)) cfg fps

-- | Reading _configPackages is safe to do.
configPackages :: Config -> [String]
configPackages = _configPackages

-- | Add a package to compilation
addConfigPackage :: String -> Config -> Config
addConfigPackage pkg cfg = cfg { _configPackages = pkg : _configPackages cfg }

-- | Add several packages to compilation
addConfigPackages :: [String] -> Config -> Config
addConfigPackages fps cfg = foldl (flip addConfigPackage) cfg fps


-- | Should a strict wrapper be generated for this module?
shouldExportStrictWrapper :: ModuleName a -> Config -> Bool
shouldExportStrictWrapper (ModuleName _ m) cs = m `elem` configStrict cs
