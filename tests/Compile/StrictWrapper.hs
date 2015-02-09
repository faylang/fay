module StrictWrapper (
  f, g, h, r, clog, logInlineOnly, logSeparateOnly, logBoth,
  sumInt, sumIntWrapped, zipWithPlus, zipWithPlusWrapped,
  sumPair, sumPairWrapped, zipPairs, zipPairsWrapped
) where

import           FFI
import           Prelude

f :: Int -> Int -> Int
f x y = x + y

data R = R { i :: Int }

g :: R -> Int
g R{i=i} = i

h :: R -> R
h (R i) = R (i + 1)

r :: R
r = R 2

clog :: a -> Fay ()
clog = ffi "console.log(%1)"

-- FFI Expressions
logInlineOnly = ffi "console.log(%1)" :: a -> Fay ()

logSeparateOnly :: a -> Fay ()
logSeparateOnly = ffi "console.log(%1)"

logBoth :: a -> Fay ()
logBoth = ffi "console.log(%1)" :: a -> Fay ()

-- lists
sumInt :: [Int] -> Int
sumInt xs = sum xs

zipWithPlus :: [Int] -> [Int] -> [Int]
zipWithPlus xs ys = zipWith (+) xs ys

data IntList = IntList { list :: [Int] }

sumIntWrapped :: IntList -> Int
sumIntWrapped (IntList xs) = sumInt xs

zipWithPlusWrapped :: IntList -> IntList -> IntList
zipWithPlusWrapped (IntList x) (IntList y) = IntList $ zipWithPlus x y

-- tuples
sumPair :: (Int, Int) -> Int
sumPair (x,y) = x + y

zipPairs :: (Int, Int) -> (Int, Int) -> (Int, Int)
zipPairs (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

data IntPair = IntPair { pair :: (Int, Int) }

sumPairWrapped :: IntPair -> Int
sumPairWrapped (IntPair x) = sumPair x

zipPairsWrapped :: IntPair -> IntPair -> IntPair
zipPairsWrapped (IntPair x) (IntPair y) = IntPair $ zipPairs x y

-- You should probably not use the strict wrapper from Fay, this is just for the sake of the test.
main :: Fay ()
main = do
  ffi "console.log(Strict.StrictWrapper.f(1,2))" :: Fay ()
  ffi "console.log(Strict.StrictWrapper.g({instance:'R',i:1}))" :: Fay ()
  ffi "console.log(Strict.StrictWrapper.h({instance:'R',i:1}))" :: Fay ()
  ffi "console.log(Strict.StrictWrapper.r)" :: Fay ()
  ffi "Strict.StrictWrapper.clog(123)" :: Fay ()
  ffi "Strict.StrictWrapper.logInlineOnly('inlineOnly')" :: Fay ()
  ffi "Strict.StrictWrapper.logSeparateOnly('separateOnly')" :: Fay ()
  ffi "Strict.StrictWrapper.logBoth('both')" :: Fay ()
  
  -- lists
  (ffi "console.log(Strict.StrictWrapper.sumInt(%1))" :: [Int] -> Fay ()) [1, 2, 3]
  (ffi "console.log(Strict.StrictWrapper.sumIntWrapped(%1))" :: IntList -> Fay ()) (IntList [1, 2, 3])
  (ffi "console.log(Strict.StrictWrapper.zipWithPlus(%1, %2))" :: [Int] -> [Int] -> Fay ()) [1, 2, 3] [1, 2, 3]
  (ffi "console.log(Strict.StrictWrapper.zipWithPlusWrapped(%1, %2))" :: IntList -> IntList -> Fay ()) (IntList [1, 2, 3]) (IntList [1, 2, 3])
  
  -- tuples
  (ffi "console.log(Strict.StrictWrapper.sumPair(%1))" :: (Int, Int) -> Fay ()) (1, 3)
  (ffi "console.log(Strict.StrictWrapper.sumPairWrapped(%1))" :: IntPair -> Fay ()) (IntPair (1, 3))
  (ffi "console.log(Strict.StrictWrapper.zipPairs(%1, %2))" :: (Int, Int) -> (Int, Int) -> Fay ()) (1, 3) (1, 3)
  (ffi "console.log(Strict.StrictWrapper.zipPairsWrapped(%1, %2))" :: IntPair -> IntPair -> Fay ()) (IntPair (1, 3)) (IntPair (1, 3))
