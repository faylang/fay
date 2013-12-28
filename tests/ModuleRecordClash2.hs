module ModuleRecordClash2 where

import           FFI
import           ModuleRecordClash2_Hello

alert :: String -> Fay ()
alert = ffi "console.log(%1)"

main = alert (greeting defaultHello)
