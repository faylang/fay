{-# LANGUAGE NoImplicitPrelude #-}
module Test.Util
  ( fayPath
  , isRight
  , fromLeft
  ) where

import           Fay.Compiler.Prelude

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
firstWhereM p ins = case ins of
  [] -> return Nothing
  a:as -> p a >>= \b ->
          if b then return (Just a)
               else firstWhereM p as

-- from the package `errors`
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft (Right _) = error "fromLeft got Right"
