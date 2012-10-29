

module RecCon where

import           Language.Fay.FFI
import           Language.Fay.Prelude

data Bool = True | False

main = print (head (fix (\xs -> 123 : xs)))

print :: Double -> Fay ()
print = ffi "console.log(%1)"

head (x:xs) = x

fix f = let x = f x in x
