import           Prelude

main :: Fay ()
main = putStrLn $ show $ sum [ x*x | x <- [1::Int, 2, 3, 4, 5], let y = x + 4, y < 8]
