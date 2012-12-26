module Listlen (main) where

import Language.Fay.FFI
import Language.Fay.Prelude

go :: [Double] -> [Double]
go  [] = []
--go  [c] = [c] -- this would fix it, it matters how the result is constructed
go  (c:cs) = let placed = go cs in (place c placed : placed)

place :: Double -> [Double] -> Double
place c [] = c
--place c (_:[]) = c -- this would fix it as well
place c [_] = c

-- | Main entry point.
main :: Fay ()
main = do
  forM_ (go [1,2]) $ \c -> c `seq` return ()
  printS "OK"

printS :: String -> Fay ()
printS = ffi "console.log(%1)"
