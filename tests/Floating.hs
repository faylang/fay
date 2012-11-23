import Language.Fay.Prelude
import Language.Fay.FFI

print :: Double -> Fay ()
print = ffi "console.log(%1)"

main = do
  print $ exp 0
  print $ sqrt 4
  print $ log (exp 3)
  print $ 2 ** 2
  print $ logBase 10 100
  print $ sin (pi/2)
  print $ cos pi
