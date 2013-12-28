module Cycle where

main :: Fay ()
main = mapM_ putStrLn (take 5 (cycle ["a", "b", "c"]))
