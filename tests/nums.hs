{-# LANGUAGE NoImplicitPrelude #-}

module Nums where

import Language.Fay.FFI
import Language.Fay.Prelude

negNum = -10

main = Fay ()
main = print negNum


print :: (Num a) => a -> Fay ()
print = ffi "console.log(%1)"
