import           FFI
import           Prelude hiding (take)

main = putStrLn (showList (take 5 (let ns = 1 : map' (foo 123) ns in ns)))

foo :: Double -> Double -> Double
foo x y = x * y / 2

take :: Int -> [a] -> [a]
take 0 _      = []
take n (x:xs) = x : take (n - 1) xs


map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

showList :: [Double] -> String
showList = ffi "JSON.stringify(%1)"
