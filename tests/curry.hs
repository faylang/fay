import Language.Fay.Prelude
import Language.Fay.FFI

print :: String -> Fay ()
print = ffi "console.log(%1)"

f :: Int -> Int -> Int
f x y = x + y

g :: (Int, Int) -> Int
g (x,y) = x + y

main = do
  print $ show $ curry g 3 4
  print $ show $ uncurry f (3,4)
