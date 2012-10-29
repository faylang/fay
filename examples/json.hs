

module Console where

import           Language.Fay.FFI
import           Language.Fay.Prelude

data MyData = MyData  { xVar :: Int, yVar :: Int }
instance Foreign MyData

myData :: MyData
myData = MyData { yVar = 3, xVar = 9 }

myFunction :: MyData -> MyData -> Int
myFunction a b = (xVar a) + (xVar b)

main = do
	jsonSerialized <- toJSON myData
	jsonDeserialized <- toMyData jsonSerialized
	fromStringData <- toMyData "{\"xVar\":3,\"yVar\":-1}"
	printInt $ myFunction jsonDeserialized fromStringData

-- | Print using console.log.
print :: String -> Fay ()
print = ffi "console.log(%1)"

printBool :: Bool -> Fay ()
printBool = ffi "console.log(%1)"

printInt :: Int -> Fay ()
printInt = ffi "console.log(%1)"

toMyData :: String -> Fay MyData
toMyData = ffi "JSON.parse(%1)"

toJSON :: MyData -> Fay String
toJSON = ffi "JSON.stringify(%1)"
