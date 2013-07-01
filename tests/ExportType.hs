module ExportType (
    X (..) -- Export constructor implicitly (EThingAll)
  , Y (Y)  -- Export constructor explicitly (EThingWith)
  , Z (..) -- Export fields implicitly (EThingAll)
  , W      -- Export only type (EAbs)
  , w      -- Export fields separately (EVar)
  , V (V, v1) -- Export fields partially (EThingWith)
  ) where

import           FFI
import           Prelude

data X = X

data Y = Y

data Z = Z { z :: Double }

data W = W

w :: W
w = W

data V = V { v1 :: Double, v2 :: Double }
