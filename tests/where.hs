

module Where where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main = print $ "Hello " ++ friends ++ family
  where friends = "my friends"
        family = " and family"

print :: String -> Fay ()
print = ffi "console.log(%1)"
