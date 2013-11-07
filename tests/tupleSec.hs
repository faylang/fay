{-# LANGUAGE TupleSections #-}
import Prelude

main = do
    print $ (,2) 1
    print $ (,2,,4,,6) 1 3 5
    print $ fst (((1,),) 3) 2
