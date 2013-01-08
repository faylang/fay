import Prelude

withTwo :: (Int -> Int) -> Int
withTwo f = f 2

main :: Fay ()
main = do
  print $ (* 3) 2
  print $ (7 `div`) 2
  print $ withTwo (4*)
