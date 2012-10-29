

module Circular where

import Language.Fay.Prelude
import Language.Fay.FFI

main :: Fay ()
main = let y = x + 1
           x = y + 1
       in print y

print :: Double -> Fay ()
print = ffi "console.log(%1)"
