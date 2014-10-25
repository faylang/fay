{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -w #-}

module Prelude
  (
  -- Prelude type re-exports
   Base.Char
  ,Base.String
  ,Base.Double
  ,Base.Int
  ,Base.Integer
  ,Base.Bool(..)
  ,Base.Read
  ,Base.Show
  ,Base.Eq
  ,(==)
  ,(/=)
  -- Standard data types
  ,Maybe(..)
  ,maybe
  -- Monads
  ,(>>=)
  ,(>>)
  ,return
  ,fail
  ,when
  ,unless
  ,forM
  ,forM_
  ,mapM
  ,mapM_
  ,(=<<)
  ,sequence
  ,sequence_
  ,void
  ,(>=>)
  ,(<=<)
  -- Num
  ,(*)
  ,(+)
  ,(-)
  -- Ord
  ,Ord
  ,Ordering(..)
  -- An ordering.
  ,(<)
  ,(<=)
  ,(>)
  ,(>=)
  ,compare
  -- Enum
  ,succ
  ,pred
  ,enumFrom
  ,enumFromTo
  ,enumFromBy
  ,enumFromThen
  ,enumFromByTo
  ,enumFromThenTo
  -- Fractional
  ,(/)
  -- Integral
  ,fromIntegral
  ,fromInteger
  -- Bools
  ,(&&)
  ,(||)
  ,not
  ,otherwise
  -- Show
  ,show
  -- Errors
  ,error
  ,undefined
  ,Either(..)
  ,either
  -- Functions
  ,until
  ,($!)
  ,seq
  ,const
  ,id
  ,(.)
  ,($)
  ,flip
  ,curry
  ,uncurry
  ,snd
  ,fst
  -- Numbers
  ,div
  ,mod
  ,divMod
  ,min
  ,max
  ,recip
  ,negate
  ,abs
  ,signum
  ,pi
  ,exp
  ,sqrt
  ,log
  ,(**)
  ,(^^)
  ,unsafePow
  ,(^)
  ,logBase
  ,sin
  ,tan
  ,cos
  ,asin
  ,atan
  ,acos
  ,sinh
  ,tanh
  ,cosh
  ,asinh
  ,atanh
  ,acosh
  ,properFraction
  ,truncate
  ,round
  ,ceiling
  ,floor
  ,subtract
  ,even
  ,odd
  ,gcd
  ,quot
  ,quot'
  ,quotRem
  ,rem
  ,rem'
  ,lcm
  -- Lists
  ,find
  ,filter
  ,null
  ,map
  ,nub
  ,nub'
  ,elem
  ,notElem
  ,sort
  ,sortBy
  ,insertBy
  ,conc
  ,concat
  ,concatMap
  ,foldr
  ,foldr1
  ,foldl
  ,foldl1
  ,(++)
  ,(!!)
  ,head
  ,tail
  ,init
  ,last
  ,iterate
  ,repeat
  ,replicate
  ,cycle
  ,take
  ,drop
  ,splitAt
  ,takeWhile
  ,dropWhile
  ,span
  ,break
  ,zipWith
  ,zipWith3
  ,zip
  ,zip3
  ,unzip
  ,unzip3
  ,lines
  ,unlines
  ,words
  ,unwords
  ,and
  ,or
  ,any
  ,all
  ,intersperse
  ,prependToAll
  ,intercalate
  ,maximum
  ,minimum
  ,product
  ,sum
  ,scanl
  ,scanl1
  ,scanr
  ,scanr1
  ,lookup
  ,length
  ,length'
  ,reverse
  -- IO
  ,print
  ,putStrLn
  ,ifThenElse
  ,Fay
  )
  where

import           Data.Data
import           Fay.FFI
import           "base" Prelude   (Bool (True, False), Eq, seq, (&&), (/=),
                                   (==), (||))
import qualified "base" Prelude   as Base
#ifndef FAY
import           "base" Prelude   (Either (..), Maybe (..), Ordering (..))
#endif

--------------------------------------------------------------------------------
-- Fixities

infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -

-- The (:) operator is built-in syntax, and cannot legally be given
-- a fixity declaration; but its fixity is given by:
--   infixr 5  :

-- Provided by base prelude
--   infix  4  ==, /=
--   infixr 3  &&
--   infixr 2  ||
--   infixr 0  $, $!

infixr 4  <, <=, >=, >
infixl 1  >>, >>=
infixr 1  =<<, >=>, <=<
infixr 0  $, $!

-- PreludeList

infixl 9  !!
infixr 5  ++
infix  4  `elem`, `notElem`

--------------------------------------------------------------------------------
-- Aliases of base

type Char    = Base.Char
type Double  = Base.Double
type Int     = Base.Int
type Integer = Base.Integer
type String  = Base.String

--------------------------------------------------------------------------------
-- Standard data types

-- | Maybe type.
#ifdef FAY
data Maybe a = Just a | Nothing
instance Base.Read a => Base.Read (Maybe a)
instance Base.Show a => Base.Show (Maybe a)
instance Typeable a => Typeable (Maybe a)
instance Data a => Data (Maybe a)
#endif

-- | Either type.
#ifdef FAY
data Either a b = Left a | Right b
#endif

maybe :: t -> (t1 -> t) -> Maybe t1 -> t
maybe m _ Nothing = m
maybe _ f (Just x) = f x

--------------------------------------------------------------------------------
-- Monads

-- | Monomorphic bind for Fay.
(>>=) :: Ptr (Fay a) -> Ptr (a -> Fay b) -> Ptr (Fay b)
(>>=) = ffi "Fay$$_(Fay$$bind(%1)(%2))"

-- | Monomorphic then for Fay.
(>>) :: Ptr (Fay a) -> Ptr (Fay b) -> Ptr (Fay b)
(>>) = ffi "Fay$$_(Fay$$then(%1)(%2))"

-- | Monomorphic return for Fay.
return :: a -> Fay a
return = ffi "Fay$$return(%1)"

fail :: String -> Fay a
fail = error

when :: Bool -> Fay a -> Fay ()
when p m = if p then m >> return () else return ()

unless :: Bool -> Fay a -> Fay ()
unless p m = if p then return () else m >> return ()

forM :: [a] -> (a -> Fay b) -> Fay [b]
forM lst fn = sequence $ map fn lst

forM_ :: [a] -> (a -> Fay b) -> Fay ()
forM_ (x:xs) m = m x >> forM_ xs m
forM_ []     _ = return ()

mapM :: (a -> Fay b) -> [a] -> Fay [b]
mapM fn lst = sequence $ map fn lst

mapM_ :: (a -> Fay b) -> [a] -> Fay ()
mapM_ m (x:xs) = m x >> mapM_ m xs
mapM_ _ []     = return ()

(=<<) :: (a -> Fay b) -> Fay a -> Fay b
f =<< x = x >>= f

void :: Fay a -> Fay ()
void f = f >> return ()

(>=>) :: (a -> Fay b) -> (b -> Fay c) -> a -> Fay c
(>=>) f g x = f x >>= g

(<=<) :: (b -> Fay c) -> (a -> Fay b) -> a -> Fay c
(<=<) g f x = f x >>= g

-- | Evaluate each action in the sequence from left to right,
-- and collect the results.
sequence :: [Fay a] -> Fay [a]
sequence ms = foldr k (return []) ms
            where
              k m m' = do { x <- m; xs <- m'; return (x:xs) }

sequence_ :: [Fay a] -> Fay ()
sequence_ []     = return ()
sequence_ (m:ms) = m >> sequence_ ms

--------------------------------------------------------------------------------
-- Num

class Base.Num a => Num a where
  (*) :: a -> a -> a
  (+) :: a -> a -> a
  (-) :: a -> a -> a

instance Num Int
instance Num Double

--------------------------------------------------------------------------------
-- Ord

-- An ordering.
#ifdef FAY
data Ordering = GT | LT | EQ
#endif

class (Eq a) => Ord a where
  (<)  :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>)  :: a -> a -> Bool
  (>=) :: a -> a -> Bool

instance Ord Char
instance Ord Double
instance Ord Int
instance Ord Integer

compare :: Ord a => a -> a -> Ordering
compare x y =
  if x > y
     then GT
     else if x < y
             then LT
             else EQ

--------------------------------------------------------------------------------
-- Enum

class (Base.Enum a) => Enum a where

instance Enum Int
-- Integers are represented as JS numbers which aren't arbitrary precision
-- se we shouldn't add an Enum instance.

succ :: Num a => a -> a
succ x = x + 1

pred :: Num a => a -> a
pred x = x - 1

enumFrom :: Num a => a -> [a]
enumFrom i = i : enumFrom (i + 1)

enumFromTo :: (Ord t, Num t) => t -> t -> [t]
enumFromTo i n =
  if i > n then [] else i : enumFromTo (i + 1) n

enumFromBy :: (Num t) => t -> t -> [t]
enumFromBy fr by = fr : enumFromBy (fr + by) by

enumFromThen :: (Num t) => t -> t -> [t]
enumFromThen fr th = enumFromBy fr (th - fr)

enumFromByTo :: (Ord t, Num t) => t -> t -> t -> [t]
enumFromByTo fr by to = if by < 0 then neg fr else pos fr
  where neg x = if x < to then [] else x : neg (x + by)
        pos x = if x > to then [] else x : pos (x + by)

enumFromThenTo :: (Ord t, Num t) => t -> t -> t -> [t]
enumFromThenTo fr th to = enumFromByTo fr (th - fr) to

--------------------------------------------------------------------------------
-- Fractional

class (Num a,Base.Fractional a) => Fractional a where
  (/) :: a -> a -> a

instance Fractional Double

--------------------------------------------------------------------------------
-- Integral

class (Enum a,Base.Integral a) => Integral a

instance Integral Int
-- Can't add Integer instance since Integer isn't an Enum (see Enum above).

fromIntegral :: (Num a, Num b) => Ptr a -> Ptr b
fromIntegral = ffi "%1"

fromInteger :: Num a => Ptr Integer -> Ptr a
fromInteger = ffi "%1"

--------------------------------------------------------------------------------
-- Bools

not :: Bool -> Bool
not p = if p then False else True

otherwise :: Bool
otherwise = True

--------------------------------------------------------------------------------
-- Show

-- | Uses JSON.stringify.
show :: Automatic a -> String
show = ffi "JSON.stringify(%1)"

--------------------------------------------------------------------------------
-- Errors

-- | Throws a JavaScript error.
error :: String -> a
error = ffi "(function() { throw %1 })()"

-- | Throws "undefined" via "error".
undefined :: a
undefined = error "Prelude.undefined"

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b

--------------------------------------------------------------------------------
-- Functions

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until p f (f x)

($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x

const :: a -> b -> a
const a _ = a

id :: a -> a
id x = x

(.) :: (t1 -> t) -> (t2 -> t1) -> t2 -> t
(f . g) x = f (g x)

($) :: (t1 -> t) -> t1 -> t
f $ x = f x

flip :: (t1 -> t2 -> t) -> t2 -> t1 -> t
flip f x y = f y x

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f p = case p of (x, y) -> f x y

snd :: (t, t1) -> t1
snd (_,x) = x

fst :: (t, t1) -> t
fst (x,_) = x

--------------------------------------------------------------------------------
-- Numbers

div :: Int -> Int -> Int
div x y
  | x > 0 && y < 0 = quot (x-1) y - 1
  | x < 0 && y > 0 = quot (x+1) y - 1
div x y            = quot x y

mod :: Int -> Int -> Int
mod x y
  | x > 0 && y < 0 = rem (x-1) y + y + 1
  | x < 0 && y > 0 = rem (x+1) y + y - 1
mod x y            = rem x y

divMod :: Int -> Int -> (Int, Int)
divMod x y
  | x > 0 && y < 0 = case (x-1) `quotRem` y of (q,r) -> (q-1, r+y+1)
  | x < 0 && y > 1 = case (x+1) `quotRem` y of (q,r) -> (q-1, r+y-1)
divMod x y         = quotRem x y

min :: (Num a) => a -> a -> a
min = ffi "Math.min(Fay$$_(%1),Fay$$_(%2))"

max :: (Num a) => a -> a -> a
max = ffi "Math.max(Fay$$_(%1),Fay$$_(%2))"

recip :: Double -> Double
recip x = 1 / x

-- | Implemented in Fay.
negate :: Num a => a -> a
negate x = (-x)

-- | Implemented in Fay.
abs :: (Num a, Ord a) => a -> a
abs x = if x < 0 then negate x else x

-- | Implemented in Fay.
signum :: (Num a, Ord a) => a -> a
signum x = if x > 0 then 1 else if x == 0 then 0 else -1

-- | Uses Math.PI.
pi :: Double
pi = ffi "Math.PI"

-- | Uses Math.exp.
exp :: Double -> Double
exp = ffi "Math.exp(%1)"

-- | Uses Math.sqrt.
sqrt :: Double -> Double
sqrt = ffi "Math.sqrt(%1)"

-- | Uses Math.log.
log :: Double -> Double
log = ffi "Math.log(%1)"

-- | Uses Math.pow.
(**) :: Double -> Double -> Double
(**) = unsafePow

-- | Uses Math.pow.
(^^) :: Double -> Int -> Double
(^^) = unsafePow

-- | Uses Math.pow.
unsafePow :: (Num a,Num b) => a -> b -> a
unsafePow = ffi "Math.pow(Fay$$_(%1),Fay$$_(%2))"

-- | Implemented in Fay, it's not fast.
(^) :: Num a => a -> Int -> a
a ^ b | b < 0  = error "(^): negative exponent"
      | b == 0 = 1
      | even b = let x = a ^ (b `quot` 2) in x * x
a ^ b          = a * a ^ (b - 1)

-- | Implemented in Fay, not fast.
logBase :: Double -> Double -> Double
logBase b x = log x / log b

-- | Uses Math.sin.
sin :: Double -> Double
sin = ffi "Math.sin(%1)"

-- | Uses Math.tan.
tan :: Double -> Double
tan = ffi "Math.tan(%1)"

-- | Uses Math.cos.
cos :: Double -> Double
cos = ffi "Math.cos(%1)"

-- | Uses Math.asin.
asin :: Double -> Double
asin = ffi "Math.asin(%1)"

-- | Uses Math.atan.
atan :: Double -> Double
atan = ffi "Math.atan(%1)"

-- | Uses Math.acos.
acos :: Double -> Double
acos = ffi "Math.acos(%1)"

-- | Implemented in Fay, not fast.
sinh :: Double -> Double
sinh x = (exp x - exp (-x)) / 2

-- | Implemented in Fay, not fast.
tanh :: Double -> Double
tanh x = let a = exp x ; b = exp (-x) in (a - b) / (a + b)

-- | Implemented in Fay, not fast.
cosh :: Double -> Double
cosh x = (exp x + exp (-x)) / 2

-- | Implemented in Fay, not fast.
asinh :: Double -> Double
asinh x = log (x + sqrt(x**2 + 1))

-- | Implemented in Fay, not fast.
atanh :: Double -> Double
atanh x = log ((1 + x) / (1 - x)) / 2

-- | Implemented in Fay, not fast.
acosh :: Double -> Double
acosh x = log (x + sqrt (x**2 - 1))

-- | Implemented in Fay, not fast.
properFraction :: Double -> (Int, Double)
properFraction x = let a = truncate x in (a, x - fromIntegral a)

-- | Implemented in Fay, not fast.
truncate :: Double -> Int
truncate x = if x < 0 then ceiling x else floor x

-- | Uses Math.round.
round :: Double -> Int
round = ffi "Math.round(%1)"

-- | Uses Math.ceil.
ceiling :: Double -> Int
ceiling = ffi "Math.ceil(%1)"

-- | Uses Math.floor.
floor :: Double -> Int
floor = ffi "Math.floor(%1)"

-- | Flip (-).
subtract :: Num a => a -> a -> a
subtract = flip (-)

-- | Implemented in Fay, not fast.
even :: Int -> Bool
even x = x `rem` 2 == 0

-- | not (even x)
odd :: Int -> Bool
odd x = not (even x)

-- | Implemented in Fay, not fast.
gcd :: Int -> Int -> Int
gcd a b = go (abs a) (abs b)
  where go x 0 = x
        go x y = go y (x `rem` y)

-- | Uses quot'.
quot :: Int -> Int -> Int
quot x y = if y == 0 then error "Division by zero" else quot' x y

-- | Uses ~~(a/b).
quot' :: Int -> Int -> Int
quot' = ffi "~~(%1/%2)"

-- | (quot x y, rem x y)
quotRem :: Int -> Int -> (Int, Int)
quotRem x y = (quot x y, rem x y)

-- | Uses rem'.
rem :: Int -> Int -> Int
rem x y = if y == 0 then error "Division by zero" else rem' x y

-- | Uses %%.
rem' :: Int -> Int -> Int
rem' = ffi "%1 %% %2"

lcm :: Int -> Int -> Int
lcm _ 0 = 0
lcm 0 _ = 0
lcm a b = abs ((a `quot` (gcd a b)) * b)

--------------------------------------------------------------------------------
-- Lists

find :: (a -> Bool) -> [a] -> Maybe a
find p (x:xs) = if p x then Just x else find p xs
find _ [] = Nothing

filter :: (a -> Bool) -> [a] -> [a]
filter p (x:xs) = if p x then x : filter p xs else filter p xs
filter _ []     = []

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

notElem :: Eq a => a -> [a] -> Bool
notElem x ys = not (elem x ys)

sort :: Ord a => [a] -> [a]
sort = sortBy compare

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

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x]    = x
foldr1 f (x:xs) = f x (foldr1 f xs)
foldr1 _ []     = error "foldr1: empty list"

foldl :: (t1 -> t -> t1) -> t1 -> [t] -> t1
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs
foldl1 _ []     = error "foldl1: empty list"

(++) :: [a] -> [a] -> [a]
x ++ y = conc x y

(!!) :: [a] -> Int -> a
a !! b = if b < 0 then error "(!!): negative index" else go a b
  where go []    _ = error "(!!): index too large"
        go (h:_) 0 = h
        go (_:t) n = go t (n-1)

head :: [a] -> a
head []    = error "head: empty list"
head (h:_) = h

tail :: [a] -> [a]
tail []    = error "tail: empty list"
tail (_:t) = t

init :: [a] -> [a]
init []    = error "init: empty list"
init [a]   = []
init (h:t) = h : init t

last :: [a] -> a
last []    = error "last: empty list"
last [a]   = a
last (_:t) = last t

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = if n < 0 then []
                         else x : replicate (n-1) x

cycle :: [a] -> [a]
cycle [] = error "cycle: empty list"
cycle xs = xs' where xs' = xs ++ xs'

take :: Int -> [a] -> [a]
take 0 _  = []
take _ [] = []
take n (x:xs) = if n < 0 then []
                         else x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n xss@(x:xs) = if n < 0 then xss
                             else drop (n-1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt 0 xs     = ([], xs)
splitAt _ []     = ([], [])
splitAt n (x:xs) = if n < 0 then ([],x:xs)
                            else case splitAt (n-1) xs of (a,b) -> (x:a, b)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []     = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile p (x:xs) = if p x then dropWhile p xs else x:xs

span :: (a -> Bool) -> [a] -> ([a], [a])
span _ []     = ([], [])
span p (x:xs) = if p x then case span p xs of (a,b) -> (x:a, b) else ([], x:xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _      _      = []

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f (a:as) (b:bs) (c:cs) = f a b c : zipWith3 f as bs cs
zipWith3 _ _      _      _      = []

zip :: [a] -> [b] -> [(a,b)]
zip (a:as) (b:bs) = (a,b) : zip as bs
zip _      _      = []

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _      _      _      = []

unzip :: [(a, b)] -> ([a], [b])
unzip ((x,y):ps) = case unzip ps of (xs,ys) -> (x:xs, y:ys)
unzip []         = ([], [])

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 ((x,y,z):ps) = case unzip3 ps of (xs,ys,zs) -> (x:xs, y:ys, z:zs)
unzip3 []           = ([], [], [])

lines :: String -> [String]
lines []   = []
lines s    = case break isLineBreak s of (a, [])   -> [a]
                                         (a, _:cs) -> a : lines cs
  where isLineBreak c = c == '\r' || c == '\n'

unlines :: [String] -> String
unlines [] = []
unlines (l:ls) = l ++ '\n' : unlines ls

words :: String -> [String]
words str = words' (dropWhile isSpace str)
  where words' []  = []
        words' s = case break isSpace s of (a,b) -> a : words b
        isSpace c  = c `elem` " \t\r\n\f\v"

unwords :: [String] -> String
unwords = intercalate " "

and :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or []     = False
or (x:xs) = x || or xs

any :: (a -> Bool) -> [a] -> Bool
any _ []     = False
any p (x:xs) = p x || any p xs

all :: (a -> Bool) -> [a] -> Bool
all _ []     = True
all p (x:xs) = p x && all p xs

intersperse :: a -> [a] -> [a]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs

prependToAll :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

maximum :: (Num a) => [a] -> a
maximum [] = error "maximum: empty list"
maximum xs = foldl1 max xs

minimum :: (Num a) => [a] -> a
minimum [] = error "minimum: empty list"
minimum xs = foldl1 min xs

product :: Num a => [a] -> a
product xs = foldl (*) 1 xs

sum :: Num a => [a] -> a
sum xs = foldl (+) 0 xs

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f z l = z : case l of [] -> []
                            (x:xs) -> scanl f (f z x) xs

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 _ [] = []
scanl1 f (x:xs) = scanl f x xs

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ z [] = [z]
scanr f z (x:xs) = case scanr f z xs of (h:t) -> f x h : h : t
                                        _     -> undefined

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 _ []     = []
scanr1 _ [x]    = [x]
scanr1 f (x:xs) = case scanr1 f xs of (h:t) -> f x h : h : t
                                      _     -> undefined

lookup :: Eq a1 => a1 -> [(a1, a)] -> Maybe a
lookup _key []          =  Nothing
lookup  key ((x,y):xys) =
  if key == x
     then Just y
     else lookup key xys

length :: [a] -> Int
length xs = length' 0 xs

length' :: Int -> [a] -> Int
length' acc (_:xs) = length' (acc+1) xs
length' acc _ = acc

reverse :: [a] -> [a]
reverse (x:xs) = reverse xs ++ [x]
reverse [] = []

--------------------------------------------------------------------------------
-- IO

print :: Automatic a -> Fay ()
print = ffi "(function(x) { if (console && console.log) console.log(x) })(%1)"

putStrLn :: String -> Fay ()
putStrLn = ffi "(function(x) { if (console && console.log) console.log(x) })(%1)"

--------------------------------------------------------------------------------
-- Additions

-- | Default definition for using RebindableSyntax.
ifThenElse :: Bool -> t -> t -> t
ifThenElse p a b = if p then a else b
