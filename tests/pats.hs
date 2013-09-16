import           Prelude

main :: Fay ()
main = do
  case [1,2] of
    []    -> putStrLn "got []"
    [a]   -> putStrLn "got one value."
    [a,b] -> putStrLn "got two values."
  case [1,2] of
    (([1,2])) -> putStrLn "parens"
