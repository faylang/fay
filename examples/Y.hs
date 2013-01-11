module Y where

import FFI
import Prelude

data Bar = Bar [Int] String

printBar :: Bar -> Fay ()
printBar = ffi "console.log(%1)"

barGo = printBar (Bar [1,2,3] "Hello, Bar!")

main = barGo
