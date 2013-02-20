{-# LANGUAGE EmptyDataDecls    #-}


module Dom where

import           FFI
import           Prelude

main :: Fay ()
main = addEventListener "load" updateBody False

updateBody :: Fay ()
updateBody = do
  printList [[1,2,3],[4,5,6]]
  print thebody
  print thewindow
  setInnerHtml thebody "Hai!"
  inner <- getInnerHtml thebody
  print' ("'" ++ inner ++ "'")

printList :: [[Double]] -> Fay ()
printList = ffi "console.log(%1)"

-- | Print using window.print.
print' :: String -> Fay ()
print' = ffi "console['log'](%1)"

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"

setInnerHtml :: Element -> String -> Fay ()
setInnerHtml = ffi "%1['innerHTML']=%2"

getInnerHtml :: Element -> Fay String
getInnerHtml = ffi "%1['innerHTML']"

thebody :: Element
thebody = ffi "document.body"

thewindow :: Element
thewindow = ffi "window"

data Element
