module ExportType (
    X (..) -- Export constructor implicitly (EThingAll)
  , Y (Y)  -- Export constructor explicitly (EThingWith)
  , Z (..) -- Export fields implicitly (EThingAll)
  , W      -- Export only type (EAbs)
  , w      -- Export fields separately (EVar)
  , V (V, v1) -- Export fields partially (EThingWith)
  ) where

import           Language.Fay.FFI
import           Prelude

data X = X
instance Foreign X

data Y = Y
instance Foreign Y

data Z = Z { z :: Double }
instance Foreign Z

data W = W
instance Foreign W

w :: W
w = W

data V = V { v1 :: Double, v2 :: Double }
instance Foreign V
