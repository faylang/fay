import Language.Fay.Prelude
import Language.Fay.FFI

print :: String -> Fay ()
print = ffi "console.log(%1)"

main = do
  when (1 == 1) $ print "Expected =="
  when (1 == 2) $ print "Unexpected =="
  when (1 /= 1) $ print "Unexpected /="
  when (1 /= 2) $ print "Expected /="
