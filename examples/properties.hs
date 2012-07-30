{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dom where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main :: Fay ()
main = addEventListener "load" updateBody False

updateBody :: Fay ()
updateBody = do
  alert thebody
  alert thewindow
  setInnerHtml thebody "Hai!"
  inner <- getInnerHtml thebody
  alert' ("'" ++ inner ++ "'")

-- | Alert using window.alert.
alert' :: String -> Fay ()
alert' = ffi "console['log'](%1)" FayNone

-- | Alert using window.alert.
alert :: Foreign a => a -> Fay ()
alert = ffi "console['log'](%1)" FayNone

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)" FayNone

setInnerHtml :: Element -> String -> Fay ()
setInnerHtml = ffi "%1['innerHTML']=%2" FayNone

getInnerHtml :: Element -> Fay String
getInnerHtml = ffi "%1['innerHTML']" FayString

thebody :: Element
thebody = ffi "document.body" FayNone

thewindow :: Element
thewindow = ffi "window" FayNone

data Element
instance Foreign Element
