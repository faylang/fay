import           FFI
import           Prelude

data R = R (Defined Double)

main :: Fay ()
main = do
  printD $ Defined (1 :: Double)
  printS $ Defined "Hello, World!"
  printSS $ Defined ["Hello,","World!"]
  printD $ (Undefined :: Defined Double)
  print $ R (Defined 1)
  print $ R Undefined
  print $ r1
  print $ r2
  return ()

printD :: Defined Double -> Fay ()
printD = ffi "console.log(%1)"

printS :: Defined String -> Fay ()
printS = ffi "console.log(%1)"

printSS :: Defined [String] -> Fay ()
printSS = ffi "console.log(%1)"

r1 :: R
r1 = ffi "{ instance: 'R', slot1 : 1 }"

r2 :: R
r2 = ffi "{ instance : 'R' }"
