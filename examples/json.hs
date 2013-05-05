

module Console where

import           FFI
import           Prelude
import           Fay.Text

data MyData = MyData  { xVar :: Text, yVar :: Int }

myData :: MyData
myData = MyData { xVar = pack "asdfasd", yVar = 9 }


main = do
	jsonSerialized <- toJSON myData
	jsonDeserialized <- toMyData jsonSerialized
	fromStringData <- toMyData "{\"xVar\":3,\"yVar\":-1,\"instance\":\"MyData\"}"
	print' jsonSerialized

-- | Print using console.log.
print' :: String -> Fay ()
print' = ffi "console.log(%1)"

printBool :: Bool -> Fay ()
printBool = ffi "console.log(%1)"

printInt :: Int -> Fay ()
printInt = ffi "console.log(%1)"

toMyData :: String -> Fay MyData
toMyData = ffi "JSON.parse(%1)"

toJSON :: MyData -> Fay String
toJSON = ffi "JSON.stringify(%1)"
