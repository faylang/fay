module StrictWrapper (f) where

import Prelude
import FFI

f :: Int -> Int -> Int
f x y = x + y

main :: Fay ()
main = ffi "console.log(Strict.StrictWrapper.f(1,2))" :: Fay ()
