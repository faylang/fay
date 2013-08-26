module FromString.Dep where

import Prelude
import FromString.DepDep (myText)

myString :: String
myString = "test"

depTest :: Fay ()
depTest = print myText

