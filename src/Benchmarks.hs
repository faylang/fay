{-# LANGUAGE NoImplicitPrelude #-}

-- | The benchmarks suite. This code is written in Fay. Which may skew
-- results, but probably not.
--
-- There's a set of benchmarks in main that are run, or you can choose
-- to run just one.
--
-- For a given benchmark it will run them a few times then calculate
-- the mean, min, max and stddev of them, which should give an
-- accurate enough picture of the performance differences, for the
-- kind of speed differences that we care about at present.
--
-- Run me like this:
-- dist/build/fay/fay src/Benchmarks.hs --no-ghc && node src/Benchmarks.js

module Benchmarks where

import FFI
import Prelude

--------------------------------------------------------------------------------
-- Main entry point

main :: Fay ()
main = do
  sumBenchmark
  lengthBenchmark
  queensBenchmark

--------------------------------------------------------------------------------
-- Benchmarks

-- | Benchmark a simple tail-recursive function.
sumBenchmark :: Fay ()
sumBenchmark = runAndSummarize "sum 1000000 0" $ benchmark 5 (sum' 10000000 0)

-- A simple tail-recursive O(n) summing function.
sum' :: Double -> Double -> Double
sum' 0 acc = acc
sum' n acc = let i = (acc + n) in seq i (sum' (n - 1) i)

-- | Benchmark a simple tail-recursive function walking a list.
lengthBenchmark :: Fay ()
lengthBenchmark = do
  let a = [1..1000000]
  echo "Forcing the list ..."
  (length a) `seq` return False
  runAndSummarize "length [1..1000000]" $ benchmark 5 (length a)

-- | Benchmark the n-queens problem.
queensBenchmark :: Fay ()
queensBenchmark = do
  runAndSummarize "nqueens 11" $ benchmark 1 (nqueens 11)

-- | Solve the n-queens problem for nq.
nqueens :: Int -> Int
nqueens nq = length (gen nq) where
  gen 0 = [] : []
  gen n = concat (map (\bs -> map (\q -> q : bs)
                                  (filter (\q -> safe q 1 bs) nqs))
                      (gen (n-1)))

  nqs = [1..nq]

safe x d (q:l) = if (x /= q) && (x /= (q+d)) && (x /= (q-d))
                     then safe x (d+1) l
                     else False
safe _ _ _     = True

--------------------------------------------------------------------------------
-- Mini benchmarking library

-- | Run a benchmark and summarize the results.
runAndSummarize :: String -> Fay [Double] -> Fay ()
runAndSummarize label m = do
  echo $ "Running benchmark “" ++ label ++ "” ..."
  results <- m
  echo $ "Results: " ++ unwords (map (\x -> show x ++ "ms") results)
  echo $ "Mean " ++ show (mean results) ++ "ms, " ++
         "Min " ++ show (minimum results) ++ "ms, " ++
         "Max " ++ show (maximum results) ++ "ms, " ++
         "Stddev " ++ show (stddev results) ++
         "\n"

-- | Benchmark a double value.
benchmark :: Double -> a -> Fay [Double]
benchmark benchmarks a = do
  echo "Recording ..."
  go a 0 []
  where
    go a count results = do
      start <- getSeconds
      a `seq` return True
      end <- getSeconds
      if count == benchmarks
         then return results
         else go a (count+1) ((end-start) : results)

--------------------------------------------------------------------------------
-- Utility functions

-- unwords :: [String] -> String
-- unwords = intercalate " "

mean :: [Double] -> Double
mean xs = foldl (\x y -> x + y) 0 xs / fromIntegral (length xs)

stddev :: [Double] -> Double
stddev xs = let tmean = mean xs
            in sqrt (mean (map (\x -> square (x - tmean)) xs))

-- sqrt :: Double -> Double
-- sqrt = ffi "Math.sqrt(%1)"

-- maximum :: [Double] -> Double
-- maximum (x:xs) = foldl max x xs
-- maximum _ = 0

-- minimum :: [Double] -> Double
-- minimum (x:xs) = foldl min x xs
-- minimum _ = 0

square :: Double -> Double
square x = x * x

echo :: String -> Fay ()
echo = ffi "console.log(%1)"

getSeconds :: Fay Double
getSeconds = ffi "new Date()"
