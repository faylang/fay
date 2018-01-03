{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Test.Util
  ( fayPath
  , getRecursiveContents
  ) where

import           Fay.Compiler.Prelude

import           System.Directory
import           System.FilePath

-- Path to the fay executable, looks in cabal-dev, dist, PATH in that order.
fayPath :: IO (Maybe FilePath)
fayPath =
  (<|>) <$> firstWhereM doesFileExist [cabalDevPath, distPath] <*> usingWhich
  where
    cabalDevPath = "./cabal-dev/bin/fay"
    distPath = "./dist/build/fay/fay"
    usingWhich = fmap (concat . lines . snd) . hush <$> readAllFromProcess "which" ["fay"] ""

firstWhereM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
firstWhereM p ins = case ins of
  [] -> return Nothing
  a:as -> p a >>= \b ->
          if b then return (Just a)
               else firstWhereM p as

-- from the package `errors`
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

-- | Get all files in a folder and its subdirectories.
-- Taken from Real World Haskell
-- http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)
