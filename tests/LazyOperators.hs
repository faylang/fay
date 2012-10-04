{-# LANGUAGE NoImplicitPrelude #-}

module LazyOperators where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main :: Fay ()
main = print testFn


testFn = let f a b = snd (a/b,10) in f 1 0 -- undefined undefined

print :: Double -> Fay ()
print = ffi "console.log(%1)"
