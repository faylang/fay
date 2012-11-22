import           Language.Fay.FFI
import           Language.Fay.Prelude

main = print (case False of
               True -> "Hello!"
               _    -> "Ney!")

print :: String -> Fay ()
print = ffi "console.log(%1)"
