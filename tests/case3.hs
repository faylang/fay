module Case3 where

f x = case () of
  _ | x == 1 -> 1
    | x == 2 -> 2
    | True -> 3

main = do
  print (f 1)
  print (f 2)
  print (f 3)
