module ModuleRecordClash (main) where

import Prelude
import ModuleRecordClash.R

data R = R

main :: Fay ()
main = do
  print R
  print i
