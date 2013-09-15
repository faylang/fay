-- | Compile with `fay examples/FayFromJs.hs --strict FayFromJs`

module FayFromJs where

import           Prelude

data Vector = Vector { x :: Double , y :: Double }

aVector :: Vector
aVector = Vector 1 2

len :: Vector -> Double
len (Vector a b) = sqrt (a^^2 + b^^2)

add :: Vector -> Vector -> Vector
add (Vector a b) (Vector c d) = Vector (a+c) (b+d)
