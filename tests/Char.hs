module Char where

import FFI

main :: Fay ()
main = do
  print 'a'
  putStrLn ('a' : "bc")
  print (head "abc")
