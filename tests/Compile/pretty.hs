module Pretty where

import Prelude

main = do
  let n = 3::Int
  print . length' 0 . takeWhile (<=n) $ [0..]
  putStrLn $  if (n + 5) ^ 2 == 64 && n*8 == 26 then "T" else "F"
  forM [1..5] $ \k -> putStrLn $ if odd k then "odd" else "even"
