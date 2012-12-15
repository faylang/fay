module Main where

import Language.Fay.Prelude

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibN :: Int -> Int
fibN n = fibs !! n

main :: Fay ()
main = return ()
