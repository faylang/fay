

module Double2 where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main = print (10 + (2 * (4 / 2)))

print :: Double -> Fay ()
print = ffi "console.log(%1)"
