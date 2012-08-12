module Language.Fay.Stdlib
  (($)
  ,snd
  ,fst
  ,find
  ,any
  ,filter
  ,not
  ,null
  ,map
  ,nub
  ,elem
  ,Ordering(..)
  ,sort
  ,compare
  ,sortBy
  ,insertBy
  ,enumFrom
  ,zipWith
  ,zip
  ,flip
  ,maybe
  ,(.)
  ,(++)
  ,concat
  ,foldr
  ,foldl
  ,lookup
  ,intersperse
  ,prependToAll
  ,intercalate
  ,forM_
  ,mapM_
  ,when
  ,const)
  where

import           Prelude (Bool(..), Maybe(..), Ord, return, (+), (<), (==), (>), (>>), (||))

-- START

snd (_,x) = x
fst (x,_) = x

find p (x:xs) = if p x then Just x else find p xs
find p [] = Nothing

any p (x:xs) = if p x then True else any p xs
any p [] = False

filter p (x:xs) = if p x then x : filter p xs else filter p xs
filter _ []     = []

not p = if p then False else True

null [] = True
null _ = False

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

nub ls = nub' ls []

nub' []     _ = []
nub' (x:xs) ls =
  if elem x ls
     then nub' xs ls
     else x : nub' xs (x : ls)

elem x (y:ys)   = x == y || elem x ys
elem _ []       = False

data Ordering = GT | LT | EQ

sort :: (Ord a) => [a] -> [a]
sort = sortBy compare

compare x y =
  if x > y
     then GT
     else if x < y
             then LT
             else EQ

sortBy cmp = foldr (insertBy cmp) []

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _   x [] = [x]
insertBy cmp x ys =
  case ys of
    y:ys' ->
      case cmp x y of
         GT -> y : insertBy cmp x ys'
         _  -> x : ys

when p m = if p then m >> return () else return ()

enumFrom i = i : enumFrom (i + 1)

enumFromTo i n =
  if i == n
     then [i]
     else i : enumFromTo (i + 1) n

zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _      _      = []

zip :: [a] -> [b] -> [(a,b)]
zip (a:as) (b:bs) = (a,b) : zip as bs
zip _      _      = []

flip f x y = f y x

maybe m f Nothing = m
maybe m f (Just x) = f x

(f . g) x = f (g x)

x ++ y = conc x y
infixr 5 ++

f $ x = f x
infixr 0 $

-- | Append two lists.
conc :: [a] -> [a] -> [a]
conc (x:xs) ys = x : conc xs ys
conc []     ys = ys

concat = foldr conc []

foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

lookup _key []          =  Nothing
lookup  key ((x,y):xys) =
  if key == x
     then Just y
     else lookup key xys

intersperse             :: a -> [a] -> [a]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs

prependToAll            :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

forM_ (x:xs) m = m x >> forM_ xs m
forM_ []     _ = return ()

mapM_ m (x:xs) = m x >> mapM_ m xs
mapM_ _ []     = return ()

const :: a -> b -> a
const a _ = a
