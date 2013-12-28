module Trace where

import FFI
import Debug.Trace

fac :: Int -> Int
fac 0 = trace "fac" (traceShow 0 1)
fac n = trace "fac" (traceShow n (n * fac (n - 1)))

main :: Fay ()
main = print $ fac 5
