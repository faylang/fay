module Ratio where

import Data.Ratio

main :: Fay ()
main = do
  print $ 3 % 5
  print $ numerator (3 % 5)
  print $ denominator (3 % 5)
