module ReExportGlobally.A (x, NewTy(..)) where

import           Prelude

x :: Double
x = 1

newtype NewTy = NewTy { unNewTy :: Double }
