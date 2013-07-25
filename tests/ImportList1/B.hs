module ImportList1.B where

import           Prelude
import           FFI

x :: Double
x = 2

y :: Double
y = 2

data R = S { s1 :: Double, s2 :: Double }

r :: R
r = S 3 4

data X = Y Int

(<<>>) :: Double -> Double -> Double
(<<>>) = (+)
