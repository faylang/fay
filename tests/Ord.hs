import Language.Fay.Prelude
import Language.Fay.FFI

print :: String -> Fay ()
print = ffi "console.log(%1)"

main = do
  when (1 < 2) $ print "Expected <"
  when (1 < 1) $ print "Unexpected < (1)"
  when (2 < 1) $ print "Unexpected < (2)"
  when (1 >= 2) $ print "Unexpected >="
  when (1 >= 1) $ print "Expected >= (1)"
  when (2 >= 1) $ print "Expected >= (2)"
  when (1 > 2) $ print "Unexpected > (1)"
  when (1 > 1) $ print "Unexpected > (2)"
  when (2 > 1) $ print "Expected >"
  when (1 <= 2) $ print "Expected <= (1)"
  when (1 <= 1) $ print "Expected <= (2)"
  when (2 <= 1) $ print "Unexpected <="
  print $ show $ max 1 2
  print $ show $ min 1 2
  case compare 1 2 of
    EQ -> print "FAIL (EQ)"
    LT -> print "WIN (LT)"
    GT -> print "FAIL (GT)"
