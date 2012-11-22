import Language.Fay.FFI
import Language.Fay.Prelude

main = do print $ (-7/2)
          print $ (-7)/2
          print $ -f x/y
     where f n = n * n
           x = 5
           y = 2

print :: Double -> Fay ()
print = ffi "console.log(%1)"
