-- | This is to test tail-recursive calls are iterative.
import           Language.Fay.FFI
import           Language.Fay.Prelude

main = do
  print (sumTo 100000 0 :: Double)

sumTo 0 acc = acc
sumTo n acc = sumTo (n - 1) (acc + n)

print :: Double -> Fay ()
print = ffi "console.log(%1)"
