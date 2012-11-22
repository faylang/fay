import Language.Fay.Prelude
import Language.Fay.FFI

log :: String -> Fay ()
log = ffi "console.log(%1)"

sum :: [Double] -> Double
sum [] = 0
sum (x:xs) = x + sum xs

main :: Fay ()
main = log $ show $ sum [ x*x | x <- [1, 2, 3, 4, 5], let y = x + 4, y < 8]

