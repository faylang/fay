module StrictWrapper (f,g,h) where

import           FFI
import           Prelude

f :: Int -> Int -> Int
f x y = x + y

data R = R { i :: Int }

g :: R -> Int
g R{i=i} = i

h :: R -> R
h (R i) = R (i + 1)

main :: Fay ()
main = do
  ffi "console.log(Strict.StrictWrapper.f(1,2))" :: Fay ()
  ffi "console.log(Strict.StrictWrapper.g({instance:'R',i:1}))" :: Fay ()
  ffi "console.log(Strict.StrictWrapper.h({instance:'R',i:1}))" :: Fay ()
