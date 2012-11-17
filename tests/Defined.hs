module Defined where

import           Language.Fay.FFI
import           Language.Fay.Prelude

data R = R (Defined Double)
instance Foreign R

main :: Fay ()
main = do
  printD $ Defined (1 :: Double)
  printD $ (Undefined :: Defined Double)
  print $ R (Defined 1)
  print $ R Undefined
  return ()

print :: Foreign f => f -> Fay ()
print = ffi "console.log(%1)"

printD :: Foreign f => Defined f -> Fay ()
printD = ffi "console.log(%1)"
