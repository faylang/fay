{-# LANGUAGE NoImplicitPrelude #-}

module Alert where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main :: Fay ()
main = alert "Hello, World!"

-- | Alert using window.alert.
alert :: String -> Fay ()
alert = ffi "window.alert(%1)"
