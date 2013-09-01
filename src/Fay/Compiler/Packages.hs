{-# LANGUAGE TupleSections #-}

-- | Dealing with Cabal packages in Fay's own special way.

module Fay.Compiler.Packages where

import           Fay.Compiler.Config
import           Fay.Control.Monad.Extra
import           Fay.System.Process.Extra
import           Fay.Types
import           Paths_fay

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Version
import           GHC.Paths
import           System.Directory
import           System.FilePath

-- | Given a configuration, resolve any packages specified to their
-- data file directories for importing the *.hs sources.
resolvePackages :: CompileConfig -> IO CompileConfig
resolvePackages config =
  foldM resolvePackage config (configPackages config)

-- | Resolve package.
resolvePackage :: CompileConfig -> String -> IO CompileConfig
resolvePackage config name = do
  desc <- describePackage (configPackageConf config) name
  case packageVersion desc of
    Nothing -> error $ "unable to find package version: " ++ name
    Just ver -> do
      let nameVer = name ++ "-" ++ ver
      shareDir <- if isJust (configBasePath config) && name == "fay-base"
                    then return . fromJust $ configBasePath config
                    else fmap ($ nameVer) getShareGen
      let includes = [shareDir,shareDir </> "src"]
      exists <- mapM doesSourceDirExist includes
      if or exists
         then return (addConfigDirectoryIncludes (map (Just nameVer,) includes) config)
         else error $ "unable to find (existing) package's share dir: " ++ name ++ "\n" ++
                      "tried: " ++ unlines includes ++ "\n" ++
                      "but none of them seem to have Haskell files in them."

-- | Does a directory exist and does it contain any Haskell sources?
doesSourceDirExist :: FilePath -> IO Bool
doesSourceDirExist path = do
  exists <- doesDirectoryExist path
  if not exists
     then return False
     else do files <- filter (\v -> v /= "." && v /= "..") <$> getDirectoryContents path
             sub   <- anyM doesSourceDirExist $ map (path </>) files
             return $ any ((==".hs") . takeExtension) files || sub

-- | Describe the given package.
describePackage :: Maybe FilePath -> String -> IO String
describePackage db name = do
  result <- readAllFromProcess ghc_pkg args ""
  case result of
    Left  (err,_out) -> error $ "ghc-pkg describe error:\n" ++ err
    Right (_err,out) -> return out

  where args = ["describe",name] ++ ["-f" ++ db' | Just db' <- [db]]

-- | Get the package version from the package description.
packageVersion :: String -> Maybe String
packageVersion = fmap (dropWhile (==' ')) . lookup "version:" . map (span (/=' ')) . lines

-- | Make a share directory generator.
getShareGen :: IO (String -> FilePath)
getShareGen = do
  dataDir <- getDataDir
  return $ \pkg ->
    joinPath (map (replace pkg . dropTrailingPathSeparator) (splitPath dataDir))

  where replace pkg component
          | component == nameVer = pkg
          | otherwise = component
        nameVer = "fay-" ++ intercalate "." (map show (versionBranch version))
