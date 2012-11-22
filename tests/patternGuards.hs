{-# LANGUAGE FlexibleInstances #-}

-- | As pattern matches

import           Language.Fay.FFI
import           Language.Fay.Prelude

isPositive :: Double -> Bool
isPositive x | x > 0 = True
             | x <= 0 = False

threeConds x | x > 1 = 2
             | x == 1 = 1
             | x < 1 = 0

withOtherwise x | x > 1 = True
                | otherwise = False

-- Not called, throws "non-exhaustive guard"
nonExhaustive x | x > 1 = True

printD :: [Double] -> Fay ()
printD = ffi "console.log(%1)"
printB :: [Bool] -> Fay ()
printB = ffi "console.log(%1)"

main :: Fay ()
main = do
  printB [isPositive 1, isPositive 0]
  printD [threeConds 3, threeConds 1, threeConds 0]
  printB [withOtherwise 2, withOtherwise 0]
