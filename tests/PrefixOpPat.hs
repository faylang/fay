module PrefixOpPat where

f ((:) x y) = x

main :: Fay ()
main = do
  print $ f [1,2]
