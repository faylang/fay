

module RecordImport_Import where

import           Language.Fay.FFI
import           Language.Fay.Prelude

import           RecordImport_Export

f :: R -> R
f (R i) = R i

g :: Fields -> Fields
g Fields { fieldFoo = a, fieldBar = b } =
  Fields { fieldFoo = a, fieldBar = b }

showR :: R -> String
showR (R i) = "R " ++ show i

showFields :: Fields -> String
showFields (Fields a b) = "Fields " ++ show a ++ " " ++ show b

printS :: String -> Fay ()
printS = ffi "console.log(%1)"

main = do
  printS $ showR $ R 1
  printS $ showFields $ Fields { fieldFoo = 2, fieldBar = 3 }

