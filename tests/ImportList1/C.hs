module ImportList1.C where

import           Prelude

data A = B1 { b1 :: Double } | B2 Double

data UnimportedX = UnimportedY

unimportedF :: Double
unimportedF = 1
