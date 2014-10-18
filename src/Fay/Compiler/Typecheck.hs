-- | Type-check using GHC.

module Fay.Compiler.Typecheck where

import           Fay.Compiler.Prelude

import           Fay.Compiler.Defaults
import           Fay.Compiler.Misc
import           Fay.Config
import           Fay.Types

import qualified GHC.Paths             as GHCPaths

import           System.Directory

-- | Call out to GHC to type-check the file.
typecheck :: Config -> FilePath -> IO (Either CompileError String)
typecheck cfg fp = do
  faydir <- faySourceDir
  let includes = configDirectoryIncludes cfg

  -- Remove the fay source dir from includeDirs to prevent errors on FFI instance declarations.
  let includeDirs = map snd . filter ((/= faydir) . snd) . filter (isNothing . fst) $ includes
  let packages = nub . map (fromJust . fst) . filter (isJust . fst) $ includes

  ghcPackageDbArgs <-
    case configPackageConf cfg of
      Nothing -> return []
      Just pk -> do
        flag <- getGhcPackageDbFlag
        return [flag ++ '=' : pk]
  let flags =
          [ "-fno-code"
          , "-hide-package base"
          , "-cpp", "-pgmPcpphs", "-optP--cpp"
          , "-optP-C" -- Don't let hse-cpp remove //-style comments.
          , "-DFAY=1"
          , "-main-is"
          , "Language.Fay.DummyMain"
          , "-i" ++ intercalate ":" includeDirs
          , fp ] ++ ghcPackageDbArgs ++ wallF ++ map ("-package " ++) packages
  exists <- doesFileExist GHCPaths.ghc
  res <- readAllFromProcess (if exists then GHCPaths.ghc else "ghc") flags ""
  either (return . Left . GHCError . fst) (return . Right . fst) res
   where
    wallF | configWall cfg = ["-Wall"]
          | otherwise = []
