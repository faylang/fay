{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dom where

import Language.Fay.FFI
import Language.Fay.Prelude

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
alert' = foreignFay "console.log" FayNone

-- | Alert using window.alert.
alert :: Foreign a => a -> Fay ()
alert = foreignFay "console.log" FayNone
  
addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = foreignFay "window.addEventListener" FayNone

setInnerHtml :: Element -> String -> Fay ()
setInnerHtml = foreignSetProp "innerHTML"

getInnerHtml :: Element -> Fay String
getInnerHtml = foreignPropFay "innerHTML" FayString

thebody :: Element
thebody = foreignPure "document.body" FayNone

thewindow :: Element
thewindow = foreignPure "window" FayNone

data Element
instance Foreign Element
