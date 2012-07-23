{-# LANGUAGE NoImplicitPrelude #-}

module Fib where

import Language.Fay.FFI
import Language.Fay.Prelude

main = do
  benchmark
  benchmark
  benchmark
  benchmark  

benchmark = do
  start <- getSeconds
  print (sum 1000000 0 :: Double)
  end <- getSeconds
  print (show (end-start) ++ "ms")

sum 0 acc = acc
sum n acc = sum (n - 1) (acc + n)

getSeconds :: Fay Double
getSeconds = foreignFay "new Date" ""

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""
