import Prelude
import FFI

func :: Bool -> Int -> Int -> Int
func a b c = if a then b else c

semiAutomatic :: Ptr (Bool -> Int -> Int -> Int) -> Bool -> Int
semiAutomatic = ffi "(function () { return Fay$$fayToJs(['function', 'automatic_function'], %1)(%2, 1, 2); })()"

automatic :: Ptr (Bool -> Int -> Int -> Int) -> Bool -> Int
automatic = ffi "(function () { return Fay$$fayToJs(['automatic'], %1)(%2, 1, 2); })()"

print' :: Ptr a -> Fay ()
print' = ffi "console.log(Fay$$_(%1))"

main :: Fay ()
main = do
  print' (semiAutomatic func True)
  print' (semiAutomatic func False)
  print' (automatic func True)
  print' (automatic func False)

