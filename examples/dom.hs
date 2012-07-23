{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dom where

import Language.Fay.FFI
import Language.Fay.Prelude

main :: Fay ()
main = addEventListener "load" printBody False

printBody :: Fay ()
printBody = do
  result <- documentGetElements "body"
  print result

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""

data Element
instance Foreign Element

documentGetElements :: String -> Fay [Element]
documentGetElements =
  foreignFay "document.getElementsByTagName"
             "array"

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener =
    foreignFay "window.addEventListener" ""
