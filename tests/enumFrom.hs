

module EnumFrom where

import Language.Fay.FFI
import Language.Fay.Prelude

main :: Fay ()
main = forM_ [1..5] $ \i -> print i

print :: Double -> Fay ()
print = ffi "console.log(%1)"
