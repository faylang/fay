-- | Nullable functions.

module Data.Nullable where

import FFI
import Prelude

-- | Convert from nullable to maybe.
fromNullable :: Nullable a -> Maybe a
fromNullable (Nullable x) = Just x
fromNullable Null = Nothing

-- | Convert from maybe to nullable.
toNullable :: Maybe a -> Nullable a
toNullable (Just x) = Nullable x
toNullable Nothing = Null
