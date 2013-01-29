{-# LANGUAGE EmptyDataDecls    #-}


module Dom where

import           FFI
import           Prelude

main :: Fay ()
main = addEventListener "load" printBody False

printBody :: Fay ()
printBody = do
  result <- documentGetElements "body"
  print result

data Element
instance Foreign Element
instance Show (Element)

documentGetElements :: String -> Fay [Element]
documentGetElements = ffi "document.getElementsByTagName(%1)"

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
