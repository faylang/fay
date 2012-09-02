{-# LANGUAGE NoImplicitPrelude #-}

module AsPatternMatch where

import           Language.Fay.FFI
import           Language.Fay.Prelude

matchSame :: [a] -> ([a],[a])
matchSame x@y = (x,y)

matchSplit :: [a] -> ([a],a,[a])
matchSplit x@(y:z) = (x,y,z)

matchNested :: (a, [b]) -> ([b],b,[b])
matchNested (a,b@(x:xs)) = (b,x,xs)

print :: String -> Fay ()
print = ffi "console.log(%1)"

main :: Fay ()
main = do
  print $ show $ matchSame [1,2,3]
  print $ show $ matchSplit [1,2,3]
  print $ show $ matchNested (1, [1,2,3])
