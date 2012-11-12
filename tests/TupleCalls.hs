module TupleCalls where

import Language.Fay.FFI
import Language.Fay.Prelude

f :: (Double,Double) -> Fay ()
f (a,b) = do
  print a
  print b

main = do
  f (1,2)
  f (3,4)

print :: Foreign f => f -> Fay ()
print = ffi "console.log(%1)"
