module Fay.Compiler.Typecheck where

import           Control.Monad.IO
import           Data.List
import           Data.Maybe
import           Fay.Compiler.Misc
import           Fay.Types
import qualified GHC.Paths                       as GHCPaths
import           System.Process.Extra

typecheck :: Maybe FilePath -> Bool -> String -> Compile ()
typecheck packageConf wall fp = do
  cfg <- config id
  faydir <- io faySourceDir
  let includes = configDirectoryIncludes cfg

  -- Remove the fay source dir from includeDirs to prevent errors on FFI instance declarations.
  let includeDirs = map snd . filter ((/= faydir) . snd) . filter (isNothing . fst) $ includes
  let packages = nub . map (fromJust . fst) . filter (isJust . fst) $ includes

  ghcPackageDbArgs <-
    case packageConf of
      Nothing -> return []
      Just pk -> do
        flag <- io getGhcPackageDbFlag
        return [flag ++ '=' : pk]
  let flags =
          [ "-fno-code"
          , "-hide-package base"
          , "-cpp", "-DFAY=1"
          , "-main-is"
          , "Language.Fay.DummyMain"
          , "-i" ++ concat (intersperse ":" includeDirs)
          , fp ] ++ ghcPackageDbArgs ++ wallF ++ map ("-package " ++) packages
  res <- io $ readAllFromProcess GHCPaths.ghc flags ""
  either error (warn . fst) res
   where
    wallF | wall = ["-Wall"]
          | otherwise = []
