module CorrectTypeSigFFI where

import FFI

an_int = 0
an_int :: Int

a_string = ffi "\"hello, world\""
a_string :: String

main = putStrLn a_string
