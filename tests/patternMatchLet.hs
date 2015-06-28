module PatternMatchLet where

first3 :: String -> Fay ()
first3 cs = do
  let (a:b:c:_) = cs
  putStrLn [a, b, c]

main :: Fay ()
main = first3 "abcd"

