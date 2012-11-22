-- compile with fay --html-wrapper
-- error thrown as soon as HTML page is loaded:
-- Uncaught TypeError: Cannot read property 'car' of null

import Language.Fay.Prelude
import Language.Fay.FFI

log :: String -> Fay ()
log = ffi "console.log(%1)"

main :: Fay ()
main = log doTest

doTest :: String
doTest = case ("x","") of
    (x : xs, c) -> "OK."
