-- | Functions for the 'Defined' type.

module Data.Defined where

import FFI

-- | Convert from defined to maybe.
fromDefined :: Defined a -> Maybe a
fromDefined (Defined x) = Just x
fromDefined Undefined = Nothing

-- | Convert from maybe to defined.
toDefined :: Maybe a -> Defined a
toDefined (Just x) = Defined x
toDefined Nothing = Undefined
