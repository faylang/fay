module ListEq where

main :: Fay ()
main = do
  print $ []      == []
  print $ [1,2,3] == [1,2,3]
  print $ ["a"]   == ["b"]
