{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}
-- | Dealing with Cabal packages in Fay's own special way.
module Fay.Compiler.Packages where

import           Fay.Compiler.Prelude

import           Fay.Config
import           Paths_fay

import           Data.Version
import           GHC.Paths
import           System.Directory
import           System.FilePath
import           System.Environment

-- | Given a configuration, resolve any packages specified to their
-- data file directories for importing the *.hs sources.
resolvePackages :: Config -> IO Config
resolvePackages config =
  foldM resolvePackage config (configPackages config)

-- | Resolve package.
resolvePackage :: Config -> String -> IO Config
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
         else error $ concat
                [ "unable to find (existing) package's share dir: ", name, "\n"
                , "tried: ", unlines includes, "\n"
                , "but none of them seem to have Haskell files in them.\n"
                , "If you are using a sandbox you need to specify the HASKELL_PACKAGE_SANDBOX environment variable or use --package-conf."
                ]

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
  exists <- doesFileExist ghc_pkg
  stackInNixShell <- fmap isJust (lookupEnv "STACK_IN_NIX_SHELL")
  let command = if exists
        then if (isInfixOf ".stack" ghc_pkg || stackInNixShell)
             then "stack"
             else ghc_pkg
        else "ghc-pkg"
      extraArgs = case command of
        "stack" -> ["exec","--","ghc-pkg"]
        _       -> []
      args = extraArgs ++ ["describe",name] ++ ["--expand-env-vars", "-v2"]
             ++ ["--package-db=" ++ db' | Just db' <- [db]]
  when stackInNixShell (unsetEnv "STACK_IN_NIX_SHELL")
  result <- readAllFromProcess command args ""
  when stackInNixShell (setEnv "STACK_IN_NIX_SHELL" "1")
  case result of
    Left  (err,out) -> error $ "ghc-pkg describe error:\n" ++ err ++ "\n" ++ out
    Right (_err,out) -> return out

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
