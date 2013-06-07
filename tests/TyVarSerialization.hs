module TyVarSerialization where

import Prelude

main :: Fay ()
main = do
  printEither (Left [1,2,3])
  printEither (Right "Yow!")
  printTuple ("Hey",["ey","baby"],1,"wanna",["know"],"if you'll be my girl")

printEither :: Either [Int] String -> Fay ()
printEither = ffi "console.log(%1)"

printTuple :: (String,[String],Int,String,[String],String) -> Fay ()
printTuple = ffi "console.log(%1)"
