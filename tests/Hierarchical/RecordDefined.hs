

module Hierarchical.RecordDefined where

import           Language.Fay.FFI
import           Language.Fay.Prelude

data Callback a = Callback Double

g (Callback a) = a
