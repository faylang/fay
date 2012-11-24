import Language.Fay.Prelude
import Language.Fay.FFI

print :: Foreign a => a -> Fay ()
print = ffi "console.log(%1)"


main = do
  print $ fst $ properFraction 1.5
  print $ snd $ properFraction 1.5
  print $ truncate (-1.5)
  print $ round (-1.5)
  print $ ceiling (-1.5)
  print $ floor (-1.5)
  return ()