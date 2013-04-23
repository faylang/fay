module HidePreludeImport where

import HidePreludeImport_Import
import Prelude hiding (last)

main :: Fay ()
main = print last
