import Language.Fay.Prelude
import Language.Fay.FFI

main = print t
  where
    t :: Bool
    t = True

print :: Bool -> Fay ()
print = ffi "console.log(%1)"
