import Language.Fay.Prelude

f n | n <= 0 = 0
    | n >= 10 = 11
f n          = n + 1

main = do
  print $ f (-1)
  print $ f 12
  print $ f 1

