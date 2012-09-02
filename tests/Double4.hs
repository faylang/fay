{-# LANGUAGE NoImplicitPrelude #-}

module Double4 where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main = print 1

print :: Double -> Fay ()
print = ffi "console.log(%1)"
