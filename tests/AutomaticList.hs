module AutomaticList where

import Prelude
import FFI

main :: Fay ()
main = do
  printA ()
  printA ([] :: [Int])
  printA [1,2,3]
  printA [[1],[2],[3]]
  printA (1,2)
  printA ([1],[2])
  printArr [1,2,3]
  printArr [[1],[2],[3]]
  printT (1,2)
  printT ([1],[2])
  printA $ readA "[]"
  printA . tail $ readA "[1,2,3]"
  printA . tail $ readArr "[1,2,3]"
  printA . snd $ readT "[1,2]"

printA :: Automatic a -> Fay ()
printA = ffi "console.log(%1)"

printArr :: Automatic [a] -> Fay ()
printArr = ffi "console.log(%1)"

printT :: (Automatic a, Automatic b) -> Fay ()
printT = ffi "console.log(%1)"

readA :: String -> Automatic a
readA = ffi "JSON.parse(%1)"

readArr :: String -> Automatic [a]
readArr = ffi "JSON.parse(%1)"

readT :: String -> Automatic (a,b)
readT = ffi "JSON.parse(%1)"
