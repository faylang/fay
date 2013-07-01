module Ratio where

import Prelude
import Data.Ratio

main :: Fay ()
main = do
  print $ 3 % 5
  print $ numerator (3 % 5)
  print $ denominator (3 % 5)
