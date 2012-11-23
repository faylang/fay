import           Language.Fay.FFI
import           Language.Fay.Prelude

import           ExportType

w' :: W
w' = w

main :: Fay ()
main = do
  print X
  print Y
  print (Z 1)
  print (z (Z 1))
  print w'
  print (V 1 2)
  print (v1 (V 1 2))

print :: Foreign f => f -> Fay ()
print = ffi "console.log(%1)"
