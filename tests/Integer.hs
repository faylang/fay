module Integer where

main :: Fay ()
main = do
  -- Integer is Ord, Eq, and Show, but not Num.
  print $ (1::Integer) < 1000
  print $ (1::Integer) > 1000
  print $ (1::Integer) == 1000
  print $ (3::Integer) == 3
  print $ 1024
