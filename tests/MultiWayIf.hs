{-# LANGUAGE MultiWayIf #-}
module MultiWayIf where

f :: Int -> Char
f x = if | x == 1    -> 'a'
         | x == 2    -> 'b'
         | otherwise -> 'c'

main :: Fay ()
main = do
  print (f 1)
  print (f 2)
  print (f 3)
