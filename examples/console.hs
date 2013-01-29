module Console (main) where

import Prelude

main = putStrLn (showInt (fib 10))

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

showInt :: Int -> String
showInt = ffi "%1+''"
