module Ord where

import           Prelude

main = do
  when ((1::Int) < 2) $ putStrLn "Expected <"
  when ((1::Int) < 1) $ putStrLn "Unexpected < (1)"
  when ((2::Int) < 1) $ putStrLn "Unexpected < (2)"
  when ((1::Int) >= 2) $ putStrLn "Unexpected >="
  when ((1::Int) >= 1) $ putStrLn "Expected >= (1)"
  when ((2::Int) >= 1) $ putStrLn "Expected >= (2)"
  when ((1::Int) > 2) $ putStrLn "Unexpected > (1)"
  when ((1::Int) > 1) $ putStrLn "Unexpected > (2)"
  when ((2::Int) > 1) $ putStrLn "Expected >"
  when ((1::Int) <= 2) $ putStrLn "Expected <= (1)"
  when ((1::Int) <= 1) $ putStrLn "Expected <= (2)"
  when ((2::Int) <= 1) $ putStrLn "Unexpected <="
  print $ max 1 (2::Int)
  print $ min 1 (2::Int)
  case compare 1 (2::Int) of
    EQ -> putStrLn "FAIL (EQ)"
    LT -> putStrLn "WIN (LT)"
    GT -> putStrLn "FAIL (GT)"
