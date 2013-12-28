-- | This is to test tail-recursive calls are iterative.
module TailRecursion where

main = do
  print (sumTo 1000 0 :: Double)

sumTo 0 acc = acc
sumTo n acc = sumTo (n - 1) (acc + n)
