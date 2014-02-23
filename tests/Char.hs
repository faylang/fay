module Char where

import FFI

import Data.Char

main :: Fay ()
main = do
  print 'a'
  putStrLn ('a' : "bc")
  print (head "abc")
  print (ord 'a')
  print (chr 97)
  print (chr (ord 'a'))
  print (ord (chr 97))
