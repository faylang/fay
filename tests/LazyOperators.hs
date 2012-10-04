{-# LANGUAGE NoImplicitPrelude #-}

module LazyOperators where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main :: Fay ()
main = do
     let lazy = fnLoop lazyAdder 3
         strict = fnLoop strictAdder 3
     print lazy
     print strict
     print dontEval

dontEval = let f a b = snd (a/b,10) in f 1 0 -- undefined undefined

plus a b = a + b

lazyAdder :: (Double, Double) -> (Double, Double)
lazyAdder tup  = let (c, d) = tup in (c `plus` d, c)

strictAdder :: (Double, Double) -> (Double, Double)
strictAdder tup  = let (c, d) = tup in (c + d, c)

-- ArrowLoop instance for (->)
fnLoop :: ((t1, t2) -> (t, t2)) -> t1 -> t
fnLoop f b = let (c,d) = f (b,d) in c

print :: Double -> Fay ()
print = ffi "console.log(%1)"
