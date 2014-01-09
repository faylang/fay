module DesugarFFI where

import Prelude
import FFI

-- top-level FFI call with multi type signature
addOne, addTwo :: Int -> Int

addOne = ffi "%1 + 1"
addTwo = ffi "%1 + 2"

-- FFI call in a let binding
addThree :: Int -> Int
addThree x =
  let go :: Int -> Int
      go = ffi "%1 + 3"
  in go x

-- FFI call in a where binding
addFour :: Int -> Int
addFour x = go x
  where
  go :: Int -> Int
  go = ffi "%1 + 4"

main = do
  let result = addOne . addTwo .  addThree . addFour $ 0
  putStrLn $ show result
