import Prelude

import FFI

main :: Fay ()
main =
      case [1,2] of
        []    -> alert "got []"
        [a]   -> alert "got one value."
        [a,b] -> alert "got two values."

alert :: String -> Fay ()
alert = ffi "console.log(%1)"
