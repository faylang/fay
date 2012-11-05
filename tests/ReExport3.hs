module ReExport3 where

import Language.Fay.Prelude
import Language.Fay.FFI
import ReExport2

print :: Double -> Fay ()
print = ffi "console.log(%1)"

main :: Fay ()
main = print x
