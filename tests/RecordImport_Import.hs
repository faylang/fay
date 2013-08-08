{- NOTE: This file is also used in the Compile tests. -}

import           Prelude
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

main = do
  putStrLn $ showR $ R 1
  putStrLn $ showFields $ Fields { fieldFoo = 2, fieldBar = 3 }
