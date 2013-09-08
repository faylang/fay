module Circular where

import           Prelude

main :: Fay ()
main = let y = x + 1::Double
           x = y + 1
       in print y
