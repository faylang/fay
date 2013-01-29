

-- !!! count the number of solutions to the "n queens" problem.

module NQueens where

import Prelude
import FFI

main :: Fay ()
main = benchmark $ print (nsoln 11)

listLength :: [a] -> Int -> Int
listLength [] acc = acc
listLength (_:l) acc = listLength l (1 + acc)

listFilter :: (a -> Bool) -> [a] -> [a]
listFilter f [] = []
listFilter f  (a : l)
    | f a       = a : (listFilter f l)
    | otherwise = listFilter f l

nsoln :: Int -> Int
nsoln nq = listLength (gen nq) 0
 where
    safe :: Int -> Int -> [Int] -> Bool
    safe x d []        = True
    safe x d ( q : l) = (x /= q) && (x /= (q+d)) && (x /= (q-d)) && safe x (d+1) l

    gen :: Int -> [[Int]]
    gen 0 =  [] : []
    gen n = concat $ map (\bs -> map (\q ->  q : bs) $ listFilter (\q -> safe q 1 bs) ([1..nq])) (gen (n-1))

benchmark m = do
  start <- getSeconds
  m
  end <- getSeconds
  printS (show (end-start) ++ "ms")

printS :: String -> Fay ()
printS = ffi "console.log(%1)"

getSeconds :: Fay Double
getSeconds = ffi "new Date()"
