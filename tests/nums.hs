{-# LANGUAGE NoImplicitPrelude #-}

module Nums where

import Language.Fay.FFI
import Language.Fay.Prelude


main = Fay ()
main = do
    print -10


print :: (Num a) => a -> Fay ()
print = ffi "console.log(%1)"
