module Console (main) where

import Prelude
import FFI

main = putStrLn (showInt (fib 10))

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

showInt :: Int -> String
showInt = ffi "%1+''"
