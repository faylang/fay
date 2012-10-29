

module String where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main = print "Hello, World!"

print :: String -> Fay ()
print = ffi "console.log(%1)"
