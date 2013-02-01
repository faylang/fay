module ImportList where

import           Prelude
import           FFI

import           ImportList1.A        (x)
import           ImportList1.B        (y, R (s1), r, s2, X (Y), (<<>>))

main :: Fay ()
main = do
  print x
  print y
  print $ s1 r
  print $ s2 r
  print $ Y 1
  print $ 2 <<>> 3
