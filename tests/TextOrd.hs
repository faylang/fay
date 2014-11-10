{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RebindableSyntax #-}
module TextOrd where

import Prelude

import Data.Text

main :: Fay ()
main = do
  print ("abc" < ("c"::Text))
  print ("a" < ("bcd"::Text))
  print ("bcd" > ("a"::Text))
  print ("bcd" < ("a"::Text))
