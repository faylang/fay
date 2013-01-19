module ExportList_A (x, A (..), B (B, b1), module ExportList_B) where

import ExportList_B
import Prelude

x :: Double
x = 1

data A = A { a1 :: Double }

data B = B { b1 :: Double }
