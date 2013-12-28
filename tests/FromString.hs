{-# LANGUAGE OverloadedStrings, RebindableSyntax #-}
module FromString where

import FromString.FayText
import FromString.Dep (myString, depTest)

main :: Fay ()
main = do
  print ("This is not a String" :: Text)
  print "This is not a String"
  putStrLn myString
  print myString
  depTest
