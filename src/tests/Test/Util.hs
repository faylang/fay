module Test.Util
  ( fayPath
  , isRight
  , fromLeft
  ) where

import           Fay.System.Process.Extra (readAllFromProcess)

import           Control.Applicative
import           Prelude                  hiding (pred)
import           System.Directory

-- Path to the fay executable, looks in cabal-dev, dist, PATH in that order.
fayPath :: IO (Maybe FilePath)
fayPath =
  (<|>) <$> firstWhereM doesFileExist [cabalDevPath, distPath] <*> usingWhich
  where
    cabalDevPath = "./cabal-dev/bin/fay"
    distPath = "./dist/build/fay/fay"
    usingWhich = fmap (concat . lines . snd) . hush <$> readAllFromProcess "which" ["fay"] ""

firstWhereM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
firstWhereM pred ins = case ins of
  [] -> return Nothing
  a:as -> pred a >>= \b ->
          if b then return (Just a)
               else firstWhereM pred as

-- from the package `errors`
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft (Right _) = error "fromLeft got Right"
