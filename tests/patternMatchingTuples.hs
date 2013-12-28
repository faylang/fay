module PatternMatchingTuples where

main :: Fay ()
main = putStrLn doTest

doTest :: String
doTest = case ("x","") of
    (x : xs, c) -> "OK."
