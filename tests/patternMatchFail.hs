module PatternMatchFail where

main :: Fay ()
main = putStrLn ((\a 'a' -> "OK.") 0 'b')
