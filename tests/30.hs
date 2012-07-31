{-# LANGUAGE NoImplicitPrelude #-}
module Test where

-- | As pattern matches

import Language.Fay.Prelude
import Language.Fay.FFI

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

print :: String -> Fay ()
print = ffi "console.log(%1)" FayNone

main :: Fay ()
main = do
  print $ show $ (isPositive 1, isPositive 0)
  print $ show $ (threeConds 3, threeConds 1, threeConds 0)
  print $ show $ (withOtherwise 2, withOtherwise 0)
