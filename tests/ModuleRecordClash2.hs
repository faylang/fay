module Main where

import           FFI
import           ModuleRecordClash2_Hello
import           Prelude

alert :: String -> Fay ()
alert = ffi "console.log(%1)"

main = do
    alert (greeting defaultHello)
