{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dom where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main :: Fay ()
main = addEventListener "load" printBody False

printBody :: Fay ()
printBody = do
  result <- documentGetElements "body"
  print (show result)

print :: String -> Fay ()
print = ffi "console.log(%1)"

data Element
instance Foreign Element
instance Show (Element)

documentGetElements :: String -> Fay [Element]
documentGetElements = ffi "document.getElementsByTagName(%1)"

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
