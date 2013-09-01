-- | Extra directory functions.
module Fay.System.Directory.Extra where

import           Control.Monad    (forM)
import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath  ((</>))

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
