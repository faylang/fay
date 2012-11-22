{-# LANGUAGE NoImplicitPrelude #-}
module Language.Fay.Stdlib
  (($)
  ,(++)
  ,(.)
  ,(=<<)
  ,Defined(..)
  ,Ordering(..)
  ,show
  ,fromInteger
  ,fromRational
  ,any
  ,compare
  ,concat
  ,concatMap
  ,const
  ,elem
  ,enumFrom
  ,enumFromTo
  ,enumFromThenTo
  ,fromIntegral
  ,filter
  ,find
  ,flip
  ,foldl
  ,foldr
  ,forM_
  ,fst
  ,length
  ,mod
  ,insertBy
  ,intercalate
  ,intersperse
  ,lookup
  ,map
  ,mapM_
  ,maybe
  ,not
  ,nub
  ,null
  ,otherwise
  ,prependToAll
  ,reverse
  ,sequence
  ,snd
  ,sort
  ,sortBy
  ,when
  ,zip
  ,zipWith
  ,max
  ,min)
  where

import           Language.Fay.FFI
import           Prelude          (Bool (..), Double, Eq (..), Fractional, Int,
                                   Integer, Maybe (..), Monad (..), Num ((+), (-)),
                                   Ord ((>), (<)), Rational, Show, String, (||))

show :: (Foreign a,Show a) => a -> String
show = ffi "JSON.stringify(%1)"

data Defined a = Undefined | Defined a
instance Foreign a => Foreign (Defined a)

-- There is only Double in JS.
fromInteger :: a -> a
fromInteger x = x

fromRational :: a -> a
fromRational x = x

snd :: (t, t1) -> t1
snd (_,x) = x

fst :: (t, t1) -> t
fst (x,_) = x

find :: (a -> Bool) -> [a] -> Maybe a
find p (x:xs) = if p x then Just x else find p xs
find _ [] = Nothing

any :: (t -> Bool) -> [t] -> Bool
any p (x:xs) = if p x then True else any p xs
any _ [] = False

filter :: (a -> Bool) -> [a] -> [a]
filter p (x:xs) = if p x then x : filter p xs else filter p xs
filter _ []     = []

not :: Bool -> Bool
not p = if p then False else True

null :: [t] -> Bool
null [] = True
null _ = False

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

nub :: Eq a => [a] -> [a]
nub ls = nub' ls []

nub' :: Eq a => [a] -> [a] -> [a]
nub' []     _ = []
nub' (x:xs) ls =
  if elem x ls
     then nub' xs ls
     else x : nub' xs (x : ls)

elem :: Eq a => a -> [a] -> Bool
elem x (y:ys)   = x == y || elem x ys
elem _ []       = False

data Ordering = GT | LT | EQ

sort :: Ord a => [a] -> [a]
sort = sortBy compare

compare :: Ord a => a -> a -> Ordering
compare x y =
  if x > y
     then GT
     else if x < y
             then LT
             else EQ


sortBy :: (t -> t -> Ordering) -> [t] -> [t]
sortBy cmp = foldr (insertBy cmp) []

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _   x [] = [x]
insertBy cmp x ys =
  case ys of
    [] -> [x]
    y:ys' ->
      case cmp x y of
         GT -> y : insertBy cmp x ys'
         _  -> x : ys

when :: Monad m => Bool -> m a -> m ()
when p m = if p then m >> return () else return ()

enumFrom :: Num a => a -> [a]
enumFrom i = i : enumFrom (i + 1)

enumFromTo :: (Eq t, Num t) => t -> t -> [t]
enumFromTo i n =
  if i == n
     then [i]
     else i : enumFromTo (i + 1) n

enumFromBy :: (Num t) => t -> t -> [t]
enumFromBy fr by = fr : enumFromBy (fr + by) by

enumFromThen :: (Num t) => t -> t -> [t]
enumFromThen fr th = enumFromBy fr (th - fr)

enumFromByTo :: (Ord t, Num t) => t -> t -> t -> [t]
enumFromByTo fr by to = if by < 0 then neg fr else pos fr
  where neg fr | fr < to   = []
               | otherwise = fr : neg (fr + by)
        pos fr | fr > to   = []
               | otherwise = fr : pos (fr + by)

enumFromThenTo :: (Ord t, Num t) => t -> t -> t -> [t]
enumFromThenTo fr th to = enumFromByTo fr (th - fr) to

zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _      _      = []

zip :: [a] -> [b] -> [(a,b)]
zip (a:as) (b:bs) = (a,b) : zip as bs
zip _      _      = []

flip :: (t1 -> t2 -> t) -> t2 -> t1 -> t
flip f x y = f y x

maybe :: t -> (t1 -> t) -> Maybe t1 -> t
maybe m _ Nothing = m
maybe _ f (Just x) = f x

(.) :: (t1 -> t) -> (t2 -> t1) -> t2 -> t
(f . g) x = f (g x)

(++) :: [a] -> [a] -> [a]
x ++ y = conc x y
infixr 5 ++

($) :: (t1 -> t) -> t1 -> t
f $ x = f x
infixr 0 $

-- | Append two lists.
conc :: [a] -> [a] -> [a]
conc (x:xs) ys = x : conc xs ys
conc []     ys = ys

concat :: [[a]] -> [a]
concat = foldr conc []

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = foldr ((++) . f) []

foldr :: (t -> t1 -> t1) -> t1 -> [t] -> t1
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl :: (t1 -> t -> t1) -> t1 -> [t] -> t1
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

lookup :: Eq a1 => a1 -> [(a1, a)] -> Maybe a
lookup _key []          =  Nothing
lookup  key ((x,y):xys) =
  if key == x
     then Just y
     else lookup key xys

intersperse :: a -> [a] -> [a]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs

prependToAll :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

forM_ :: Monad m => [t] -> (t -> m a) -> m ()
forM_ (x:xs) m = m x >> forM_ xs m
forM_ []     _ = return ()

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ m (x:xs) = m x >> mapM_ m xs
mapM_ _ []     = return ()

const :: a -> b -> a
const a _ = a

length :: [a] -> Int
length xs = length' 0 xs

length' :: Int -> [a] -> Int
length' acc (_:xs) = length' (acc+1) xs
length' acc _ = acc

mod :: Double -> Double -> Double
mod = ffi "%1 %% %2"

min :: Double -> Double -> Double
min = ffi "Math.min(%1,%2)"

max :: Double -> Double -> Double
max = ffi "Math.max(%1,%2)"

fromIntegral :: Int -> Double
fromIntegral = ffi "%1"

otherwise :: Bool
otherwise = True

reverse :: [a] -> [a]
reverse (x:xs) = reverse xs ++ [x]
reverse [] = []

(=<<) :: Monad m => (a -> m b) -> m a -> m b
f =<< x = x >>= f
infixl 1 =<<

-- | Evaluate each action in the sequence from left to right,
-- and collect the results.
-- sequence :: [Fay a] -> Fay [a]
sequence :: (Monad m) => [m a] -> m [a]
sequence ms = foldr k (return []) ms
            where
              k m m' = do { x <- m; xs <- m'; return (x:xs) }
