import Language.Fay.Prelude
import Language.Fay.FFI

putStrLn :: String -> Fay ()
putStrLn = ffi "console.log(%1)"

sum :: [Double] -> Double
sum [] = 0
sum (x:xs) = x + sum xs

main :: Fay ()
main = putStrLn $ show $ sum [ x*x | x <- [1, 2, 3, 4, 5], let y = x + 4, y < 8]

