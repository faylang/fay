import Language.Fay.Prelude
import Language.Fay.FFI

print :: String -> Fay ()
print = ffi "console.log(%1)"

main = do
  print $ show (succ 1 :: Int)
  print $ show (pred 1 :: Int)
