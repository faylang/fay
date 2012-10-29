

module RecordImport_Import where

import           Language.Fay.FFI
import           Language.Fay.Prelude

import           RecordImport_Export

f :: R -> R
f (R i) = R i

showR :: R -> String
showR (R i) = "R " ++ show i

printS :: String -> Fay ()
printS = ffi "console.log(%1)"

main = printS $ showR $ R 1

