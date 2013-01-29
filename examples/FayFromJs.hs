module Main where

import Prelude
import FFI

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibN :: Int -> Int
fibN n = fibs !! n

main :: Fay ()
main = return ()
