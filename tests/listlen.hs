module Listlen (main) where

import FFI
import Prelude

-- Test provided by ticket
go :: [Double] -> [Double]
go  [] = []
go  (c:cs) = let placed = go cs in (place c placed : placed)

place :: Double -> [Double] -> Double
place c [] = c
place c [_] = c

-- | Main entry point.
main :: Fay ()
main = do
  forM_ (go [1,2]) $ \c -> c `seq` return ()
  printS "OK"

  case (1 : (2 : (3 : (4 : (5 : []))))) of
    (_1:_2:_3:_4:_5:[]) -> printS "OK"

  case (1 : (2 : (3 : (4 : (5 : []))))) of
    (_1:_2:_3:_4:_5:_6:[]) -> printS "FAIL"
    _ -> printS "OK"

  case (1 : (2 : (3 : (4 : (5 : []))))) of
    (1:2:3:4:5:[]) -> printS "OK"

  case (1 : (2 : (3 : (4 : (5 : []))))) of
    (1:2:3:_) -> printS "OK"

  case (1 : (2 : (3 : (4 : (5 : []))))) of
    (1:2:3:7:8:[]) -> printS "FAIL"
    (1:2:3:4:5:[]) -> printS "OK"

  case [] of
    (1:[]) -> printS "FAIL"
    [1,2] -> printS "FAIL"
    [] -> printS "OK"

  case ([] : []) of
    ([1]:[]) -> printS "FAIL"
    [[1,2]] -> printS "FAIL"
    [[]] -> printS "OK"

printS :: String -> Fay ()
printS = ffi "console.log(%1)"
