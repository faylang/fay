

module Hierarchical.RecordDefined where

import           Language.Fay.FFI
import           Prelude

data Callback a = Callback Double

g (Callback a) = a
