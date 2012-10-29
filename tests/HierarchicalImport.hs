

module HierarchicalImport where

import           Language.Fay.FFI
import           Language.Fay.Prelude

import           Hierarchical.Export

main :: Fay ()
main = printS exported

printS :: String -> Fay ()
printS = ffi "console.log(%1)"
