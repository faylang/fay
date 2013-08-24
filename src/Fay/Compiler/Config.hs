{-# OPTIONS -fno-warn-orphans #-}

-- | Configuration functions.

module Fay.Compiler.Config where

import Data.Default
import Data.Maybe
import Fay.Types

-- | Get all include directories without the package mapping.
configDirectoryIncludePaths :: CompileConfig -> [FilePath]
configDirectoryIncludePaths = map snd . configDirectoryIncludes

-- | Get all include directories not included through packages.
nonPackageConfigDirectoryIncludePaths :: CompileConfig -> [FilePath]
nonPackageConfigDirectoryIncludePaths = map snd . filter (isJust . fst) . configDirectoryIncludes

-- | Add a mapping from (maybe) a package to a source directory
addConfigDirectoryInclude :: Maybe String -> FilePath -> CompileConfig -> CompileConfig
addConfigDirectoryInclude pkg fp cfg = cfg { configDirectoryIncludes = (pkg, fp) : configDirectoryIncludes cfg }

-- | Add several include directories.
addConfigDirectoryIncludes :: [(Maybe String,FilePath)] -> CompileConfig -> CompileConfig
addConfigDirectoryIncludes pkgFps cfg = foldl (\c (pkg,fp) -> addConfigDirectoryInclude pkg fp c) cfg pkgFps

-- | Add several include directories without package references.
addConfigDirectoryIncludePaths :: [FilePath] -> CompileConfig -> CompileConfig
addConfigDirectoryIncludePaths fps cfg = foldl (flip (addConfigDirectoryInclude Nothing)) cfg fps

-- | Add a package to compilation
addConfigPackage :: String -> CompileConfig -> CompileConfig
addConfigPackage pkg cfg = cfg { configPackages = pkg : configPackages cfg }

-- | Add several packages to compilation
addConfigPackages :: [String] -> CompileConfig -> CompileConfig
addConfigPackages fps cfg = foldl (flip addConfigPackage) cfg fps

-- | Default configuration.
instance Default CompileConfig where
  def = addConfigPackage "fay-base"
    CompileConfig
    { configOptimize           = False
    , configFlattenApps        = False
    , configExportBuiltins     = True
    , configExportRuntime      = True
    , configExportStdlib       = True
    , configExportStdlibOnly   = False
    , configDirectoryIncludes  = []
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
    , configPackages           = []
    , configBasePath           = Nothing
    , configFromString         = False
    }
