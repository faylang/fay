{-# LANGUAGE NoImplicitPrelude #-}

module Tailrecursive where

import           Language.Fay.FFI
import           Language.Fay.Prelude

benchmarks = 5

main = do
  results <- benchmark 0 []
  printS ("Results: " ++ unwords (map (\x -> show x ++ "ms") results))
  printS ("Mean " ++ show (mean results) ++ "ms, " ++
          "Min " ++ show (minimum results) ++ "ms, " ++
          "Max " ++ show (maximum results) ++ "ms, " ++
          "Stddev " ++ show (stddev results))

benchmark count results = do
  start <- getSeconds
  printD (sum 1000000 0 :: Double)
  end <- getSeconds
  if count == benchmarks
     then return results
     else benchmark (count+1) ((end-start) : results)

-- tail recursive
sum 0 acc = acc
sum n acc = sum (n - 1) (acc + n)

getSeconds :: Fay Double
getSeconds = foreignFay "new Date" FayNone

printD :: Double -> Fay ()
printD = foreignFay "console.log" FayNone

printS :: String -> Fay ()
printS = foreignFay "console.log" FayNone

sqrt :: Double -> Double
sqrt = foreignPure "Math.sqrt" FayNone

unwords = intercalate " "

mean xs = foldl (\x y -> x + y) 0 xs / length xs

stddev xs = let tmean = mean xs
            in sqrt (mean (map (\x -> square (x - tmean)) xs))

length = foldl (\acc x -> acc + 1) 0

min x y = if x > y then y else x
max x y = if x > y then x else y
maximum (x:xs) = foldl max x xs
minimum (x:xs) = foldl min x xs

square x = x * x
