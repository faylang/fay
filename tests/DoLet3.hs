module DoLet3 where

import Prelude
import FFI

data R = R Int

main = do
  print 1
  let [a] = [2]
  print a
  let (b,c) = (3,4)
  print b
  print c
  let R d = R 5
  print d
  let [e] = []
  print e
