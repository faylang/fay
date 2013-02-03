{-# LANGUAGE TupleSections              #-}

-- | Dealing with Cabal packages in Fay's own special way.

module Language.Fay.Compiler.Packages where

import Language.Fay.Types

import Control.Monad
import Data.List
import Data.Version
import GHC.Paths
import Paths_fay
import System.Directory
import System.FilePath
import System.Process.Extra

-- | Given a configuration, resolve any packages specified to their
-- data file directories for importing the *.hs sources.
resolvePackages :: CompileConfig -> IO CompileConfig
resolvePackages config = do
  foldM resolvePackage config (configPackages config)

-- | Resolve package.
resolvePackage :: CompileConfig -> String -> IO CompileConfig
resolvePackage config name = do
  desc <- describePackage (configPackageConf config) name
  case packageVersion desc of
    Nothing -> error $ "unable to find package version: " ++ name
    Just ver -> do
      let nameVer = name ++ "-" ++ ver
      shareDir <- fmap ($ nameVer) getShareGen
      let includes = [shareDir,shareDir </> "src"]
      exists <- mapM doesSourceDirExist includes
      if any id exists
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
     else do files <- getDirectoryContents path
             return $ any ((==".hs") . takeExtension) files

-- | Describe the given package.
describePackage :: Maybe FilePath -> String -> IO String
describePackage db name = do
  result <- readAllFromProcess ghc_pkg args ""
  case result of
    Left err -> error $ "ghc-pkg describe error:\n" ++ err
    Right (_err,out) -> return out

  where args = concat [["describe",name]
                      ,["-f" ++ db' | Just db' <- [db]]]

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
