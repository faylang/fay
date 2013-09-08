import           FFI
import           Prelude

data R = R (Nullable Double)

main :: Fay ()
main = do
  printD $ Nullable (1 :: Double)
  printNS $ Nullable "Hello, World!"
  printSS $ Defined ["Hello,","World!"]
  printD $ (Null :: Nullable Double)
  print' $ R (Nullable 1)
  print' $ R Null
  print' $ r1
  print' $ r2
  print' $ parseInt "3"
  print' $ parseInt "x"
  return ()

printD :: Nullable Double -> Fay ()
printD = ffi "console.log(%1)"

printNS :: Nullable String -> Fay ()
printNS = ffi "console.log(%1)"

printS :: Defined String -> Fay ()
printS = ffi "console.log(%1)"

printSS :: Defined [String] -> Fay ()
printSS = ffi "console.log(%1)"

print' :: Automatic f -> Fay ()
print' = ffi "console.log(%1)"

r1 :: R
r1 = ffi "{ instance: 'R', slot1 : 1 }"

r2 :: R
r2 = ffi "{ instance : 'R', slot1 : null }"

parseInt :: String -> Nullable Int
parseInt = ffi "(function () { var n = global.parseInt(%1, 10); if (isNaN(n)) return null; return n; })()"
