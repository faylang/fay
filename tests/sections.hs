import Language.Fay.Prelude
import Language.Fay.FFI

print :: Int -> Fay ()
print = ffi "console.log(%1)"

withTwo :: (Int -> Int) -> Int
withTwo f = f 2

main :: Fay ()
main = do
  print $ (* 3) 2
  print $ (7 `div`) 2
  print $ withTwo (4*)
