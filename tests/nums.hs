module Nums where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main :: Fay ()
main = print (-10 :: Double)

print :: Foreign f => f -> Fay ()
print = ffi "console.log(%1)"
