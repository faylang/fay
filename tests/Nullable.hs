import           Language.Fay.FFI
import           Language.Fay.Prelude

data R = R (Nullable Double)
instance Foreign R

main :: Fay ()
main = do
  printD $ Nullable (1 :: Double)
  printD $ (Null :: Nullable Double)
  print' $ R (Nullable 1)
  print' $ R Null
  print' $ r1
  print' $ r2
  return ()

printD :: Foreign f => Nullable f -> Fay ()
printD = ffi "console.log(%1)"

print' :: Foreign f => f -> Fay ()
print' = ffi "console.log(%1)"

r1 :: R
r1 = ffi "{ instance: 'R', slot1 : 1 }"

r2 :: R
r2 = ffi "{ instance : 'R', slot1 : null }"
