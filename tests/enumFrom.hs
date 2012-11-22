import Language.Fay.FFI
import Language.Fay.Prelude

take :: Int -> [a] -> [a]
take 0 _      = []
take n []     = []
take n (x:xs) = x : take (n-1) xs

main :: Fay ()
main = do
    forM_ [1..5] $ \i -> print i
    forM_ (take 5 [1..]) $ \i -> print i
    forM_ [1,3..9] $ \i -> print i
    forM_ (take 3 [1,3..]) $ \i -> print i

print :: Double -> Fay ()
print = ffi "console.log(%1)"
