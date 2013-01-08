module ImportList1.C where

import           Prelude
import           Language.Fay.FFI

data A = B1 { b1 :: Double } | B2 Double
instance Foreign A

data UnimportedX = UnimportedY

unimportedF :: Double
unimportedF = 1
