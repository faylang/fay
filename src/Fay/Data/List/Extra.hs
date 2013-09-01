-- | Extra list functions.

module Fay.Data.List.Extra where

import           Data.List hiding (map)
import           Prelude   hiding (map)

-- | Get the union of a list of lists.
unionOf :: (Eq a) => [[a]] -> [a]
unionOf = foldr union []

-- | Flip of map.
for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap
