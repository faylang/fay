import Language.Fay.Prelude
import Language.Fay.FFI

print :: String -> Fay ()
print = ffi "console.log(%1)"

raw :: Either Int Int -> Int
raw x = case x of Left a -> a + 1
                  Right b -> b + 2

func :: Either Int Int -> Int
func x = either (\x -> x + 1) (\x -> x + 2) x

main = do
  print $ show $ raw $ Left 5
  print $ show $ raw $ Right 5
  print $ show $ func $ Left 5
  print $ show $ func $ Right 5
