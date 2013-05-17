module Main (main) where

import Prelude
import ExportList_A
import FFI

a = A { a1 = 3 }
b = B { b1 = 4 }

main :: Fay ()
main = do
  print x
  print y
  print $ a1 a
  print $ b1 b
  print c
  print d

