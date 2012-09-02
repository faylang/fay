{-# LANGUAGE NoImplicitPrelude #-}

module Double3 where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main = print (5 * 3 / 2)

print :: Double -> Fay ()
print = ffi "console.log(%1)"
