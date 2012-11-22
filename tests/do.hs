import           Language.Fay.FFI
import           Language.Fay.Prelude

main = do print "Hello,"; print "World!"

print :: String -> Fay ()
print = ffi "console.log(%1)"
