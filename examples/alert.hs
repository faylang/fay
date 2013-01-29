

module Alert where

import           FFI
import           Prelude

main :: Fay ()
main = alert "Hello, World!"

-- | Alert using window.alert.
alert :: String -> Fay ()
alert = ffi "window.alert(%1)"
