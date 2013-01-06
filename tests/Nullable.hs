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
  print' $ parseInt "3"
  print' $ parseInt "x"
  return ()

printD :: Foreign f => Nullable Double -> Fay ()
printD = ffi "console.log(%1)"

print' :: Foreign f => Automatic f -> Fay ()
print' = ffi "console.log(%1)"

r1 :: R
r1 = ffi "{ instance: 'R', slot1 : 1 }"

r2 :: R
r2 = ffi "{ instance : 'R', slot1 : null }"

parseInt :: String -> Nullable Int
parseInt = ffi "(function () { var n = global.parseInt(%1, 10); if (isNaN(n)) return null; return n; })()"
