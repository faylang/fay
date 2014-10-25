{-# LANGUAGE EmptyDataDecls #-}

module Data.MutMap.Internal where

import Data.Text
import FFI
import Prelude

data KeyValI a = KeyValI Salted a

data Salted

addSalt :: Text -> Salted
addSalt = ffi "':' + %1"

unsalt :: Salted -> Text
unsalt = ffi "%1['substr'](1)"

checkSalted :: Salted -> Bool
checkSalted = ffi "%1['charAt'](0) == ':'"
