import           Language.Fay.FFI
import           Language.Fay.Prelude

main :: Fay ()
main = print True

print :: Bool -> Fay ()
print = ffi "console.log(%1)"
