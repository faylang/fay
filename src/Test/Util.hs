module Test.Util where

import           Control.Applicative
import           System.Directory
import           System.Process.Extra

-- Path to the fay executable, looks in cabal-dev, dist, PATH in that order.
fayPath :: IO (Maybe FilePath)
fayPath = do
  cabalDev <- doesFileExist cabalDevPath
  if cabalDev
    then return (Just cabalDevPath)
    else do
      dist <- doesFileExist distPath
      if dist
        then return (Just distPath)
        else either (const Nothing) (Just . concat . lines) <$> readAllFromProcess' "which" ["fay"] ""
  where
    cabalDevPath = "./cabal-dev/bin/fay"
    distPath = "./dist/build/fay/fay"

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft (Right _) = error "fromLeft got Right"
