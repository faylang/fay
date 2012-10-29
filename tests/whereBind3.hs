

module WhereBind3 where

import           Language.Fay.FFI
import           Language.Fay.Prelude

f :: String -> String
f x = friends ++ family
  where friends = x
        family = " and family"

main = print (f "my friends")

print :: String -> Fay ()
print = ffi "console.log(%1)"
