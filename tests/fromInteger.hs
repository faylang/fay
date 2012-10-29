

module FromInteger where

import           Language.Fay.FFI
import           Language.Fay.Prelude

print :: String -> Fay ()
print = ffi "console.log(%1)"

main :: Fay ()
main = print $ show $ fromInteger 5
