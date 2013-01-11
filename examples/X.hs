module X where

import FFI
import Prelude

data Foo = Foo [Int] String

printFoo :: Foo -> Fay ()
printFoo = ffi "console.log(%1)"

fooGo = printFoo (Foo [1,2,3] "Hello, Foo!")

main = fooGo
