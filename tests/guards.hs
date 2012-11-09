module Guards where

import Language.Fay.FFI
import Language.Fay.Prelude

f n = if n == (-1) then 0 else if n == 12 then 11 else n + 1

f n | n <= 0 = 0
    | n >= 10 = 11
f n          = n + 1

main = do
  print $ f (-1)
  print $ f 12
  print $ f 1

print :: Foreign f => f -> Fay ()
print = ffi "console.log(%1)"
