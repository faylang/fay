module Hierarchical.RecordDefined where

import           FFI
import           Prelude

data Callback a = Callback Double

g (Callback a) = a
