{-# LANGUAGE FlexibleInstances #-}

-- | As pattern matches

import           Language.Fay.Prelude
import           Language.Fay.FFI

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

main :: Fay ()
main = do
  putStrLn $ showList [isPositive 1, isPositive 0]
  putStrLn $ showList [threeConds 3, threeConds 1, threeConds 0]
  putStrLn $ showList [withOtherwise 2, withOtherwise 0]

showList :: [a] -> String
showList = ffi "JSON.stringify(%1)"

