module Data.List.Extra where

import Data.List hiding (map)
import Prelude hiding (map)

unionOf :: (Eq a) => [[a]] -> [a]
unionOf = foldr union []

for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap
