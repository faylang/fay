module StringForcing where

import Prelude
import FFI

main = do
  let s1 = "alma"
  let s2 = s1 ++ map id s1
  putStrLn s1
  putStrLn s2
