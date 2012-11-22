import Language.Fay.Prelude
import Language.Fay.FFI

main = let t :: Bool
           t = True
       in print t

print :: Bool -> Fay ()
print = ffi "console.log(%1)"
