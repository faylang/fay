module Num where

import           Prelude

main = do
  print (1 + 2::Int)
  print (4 - 1::Int)
  print (3 * 1::Int)
  print (negate (1 - 4::Int))
  print (abs (1 - 4::Int))
  print ((-3) * signum (-10::Int))
