-- | This is to test tail-recursive calls are iterative.

{-# LANGUAGE NoImplicitPrelude #-}

module Fib where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main = do
  print (sum 100000 0 :: Double)

sum 0 acc = acc
sum n acc = sum (n - 1) (acc + n)

getSeconds :: Fay Double
getSeconds = ffi "new Date"

print :: Double -> Fay ()
print = ffi "console.log(%1)"
