import Prelude

main :: Fay ()
main = do
    forM_ [1..5] $ \i -> print i
    forM_ (take 5 [1..]) $ \i -> print i
    forM_ [1,3..9] $ \i -> print i
    forM_ (take 3 [1,3..]) $ \i -> print i
