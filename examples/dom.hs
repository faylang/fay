{-# LANGUAGE EmptyDataDecls    #-}


module Dom where

import           FFI
import           Prelude

main :: Fay ()
main = addEventListener "load" printBody False

printBody :: Fay ()
printBody = do
  result <- documentGetElements "body"
  putStrLn (showDOM result)

data Element
instance Show (Element)

documentGetElements :: String -> Fay [Element]
documentGetElements = ffi "document.getElementsByTagName(%1)"

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"

showDOM :: [Element] -> String
showDOM = ffi "%1"
