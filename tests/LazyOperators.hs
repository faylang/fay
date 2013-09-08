module LazyOperators where

import           Prelude

main :: Fay ()
main = print testFn

testFn = let f a b = snd (a/b,10::Double) in f (1::Double) (0::Double)
