{-# LANGUAGE NoImplicitPrelude #-}
module Language.Fay.Stdlib
  (($)
  ,($!)
  ,(!!)
  ,(++)
  ,(.)
  ,(=<<)
  ,(**)
  ,(^^)
  ,(^)
  ,Defined(..)
  ,Either(..)
  ,Ordering(..)
  ,abs
  ,acos
  ,acosh
  ,all
  ,and
  ,any
  ,asin
  ,asinh
  ,asTypeOf
  ,atan
  ,atanh
  ,break
  ,ceiling
  ,compare
  ,concat
  ,concatMap
  ,const
  ,cos
  ,cosh
  ,curry
  ,cycle
  ,div
  ,divMod
  ,drop
  ,dropWhile
  ,either
  ,elem
  ,enumFrom
  ,enumFromThen
  ,enumFromThenTo
  ,enumFromTo
  ,error
  ,even
  ,exp
  ,filter
  ,find
  ,flip
  ,floor
  ,foldl
  ,foldl1
  ,foldr
  ,foldr1
  ,forM_
  ,fromInteger
  ,fromIntegral
  ,fromRational
  ,fst
  ,gcd
  ,head
  ,id
  ,init
  ,insertBy
  ,intercalate
  ,intersperse
  ,iterate
  ,last
  ,lcm
  ,length
  ,lines
  ,log
  ,logBase
  ,lookup
  ,map
  ,mapM_
  ,max
  ,maximum
  ,maybe
  ,min
  ,minimum
  ,mod
  ,negate
  ,not
  ,notElem
  ,nub
  ,null
  ,odd
  ,or
  ,otherwise
  ,pi
  ,pred
  ,prependToAll
  ,product
  ,properFraction
  ,quot
  ,quotRem
  ,recip
  ,rem
  ,repeat
  ,replicate
  ,reverse
  ,round
  ,scanl
  ,scanl1
  ,scanr
  ,scanr1
  ,seq
  ,sequence
  ,sequence_
  ,show
  ,signum
  ,sin
  ,sinh
  ,snd
  ,sort
  ,sortBy
  ,span
  ,splitAt
  ,sqrt
  ,subtract
  ,succ
  ,sum
  ,tail
  ,take
  ,takeWhile
  ,tan
  ,tanh
  ,truncate
  ,uncurry
  ,undefined
  ,unlines
  ,until
  ,unwords
  ,unzip
  ,unzip3
  ,when
  ,words
  ,zip
  ,zip3
  ,zipWith
  ,zipWith3)
  where

import           Language.Fay.FFI
import           Prelude          (Bool (..), Double, Eq (..), Fractional, Int,
                                   Integer, Maybe (..), Monad (..),
                                   Num ((+), (-), (*)), Fractional ((/)),
                                   Ord ((>), (<)), Rational, Show, String,
                                   (&&), (||), seq)

error :: String -> a
error str = case error' str of 0 -> error str ; _ -> error str

error' :: String -> Int
error' = ffi "(function() { throw %1 })()"

undefined :: a
undefined = error "Prelude.undefined"

show :: (Foreign a,Show a) => a -> String
show = ffi "JSON.stringify(%1)"

data Defined a = Undefined | Defined a
instance Foreign a => Foreign (Defined a)

data Either a b = Left a | Right b

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b

-- There is only Double in JS.
fromInteger :: a -> a
fromInteger x = x

fromRational :: a -> a
fromRational x = x

negate :: Num a => a -> a
negate x = (-x)

abs :: (Num a, Ord a) => a -> a
abs x = if x < 0 then negate x else x

signum :: (Num a, Ord a) => a -> a
signum x = if x > 0 then 1 else if x == 0 then 0 else -1

pi :: Double
pi = ffi "Math.PI"

exp :: Double -> Double
exp = ffi "Math.exp(%1)"

sqrt :: Double -> Double
sqrt = ffi "Math.sqrt(%1)"

log :: Double -> Double
log = ffi "Math.log(%1)"

(**) :: Double -> Double -> Double
(**) = unsafePow
infixr 8 **

(^^) :: Double -> Int -> Double
(^^) = unsafePow
infixr 8 ^^

unsafePow :: (Foreign a, Num a, Foreign b, Num b) => a -> b -> a
unsafePow = ffi "Math.pow(%1,%2)"

(^) :: Num a => a -> Int -> a
a ^ b | b < 0  = error "(^): negative exponent"
      | b == 0 = 1
      | even b = let x = a ^ (b `quot` 2) in x * x
a ^ b          = a * a ^ (b - 1)
infixr 8 ^

logBase :: Double -> Double -> Double
logBase b x = log x / log b

sin :: Double -> Double
sin = ffi "Math.sin(%1)"

tan :: Double -> Double
tan = ffi "Math.tan(%1)"

cos :: Double -> Double
cos = ffi "Math.cos(%1)"

asin :: Double -> Double
asin = ffi "Math.asin(%1)"

atan :: Double -> Double
atan = ffi "Math.atan(%1)"

acos :: Double -> Double
acos = ffi "Math.acos(%1)"

sinh :: Double -> Double
sinh x = (exp x - exp (-x)) / 2

tanh :: Double -> Double
tanh x = let a = exp x ; b = exp (-x) in (a - b) / (a + b)

cosh :: Double -> Double
cosh x = (exp x + exp (-x)) / 2

asinh :: Double -> Double
asinh x = log (x + sqrt(x**2 + 1))

atanh :: Double -> Double
atanh x = log ((1 + x) / (1 - x)) / 2

acosh :: Double -> Double
acosh x = log (x + sqrt (x**2 - 1))

properFraction :: Double -> (Int, Double)
properFraction x = let a = truncate x in (a, x - fromIntegral a)

truncate :: Double -> Int
truncate x = if x < 0 then ceiling x else floor x

round :: Double -> Int
round = ffi "Math.round(%1)"

ceiling :: Double -> Int
ceiling = ffi "Math.ceil(%1)"

floor :: Double -> Int
floor = ffi "Math.floor(%1)"

subtract :: Num a => a -> a -> a
subtract = flip (-)

even :: Int -> Bool
even x = x `rem` 2 == 0

odd :: Int -> Bool
odd x = not (even x)

gcd :: Int -> Int -> Int
gcd a b = go (abs a) (abs b)
  where go x 0 = x
        go x y = go y (x `rem` y)

lcm :: Int -> Int -> Int
lcm _ 0 = 0
lcm 0 _ = 0
lcm a b = abs ((a `quot` (gcd a b)) * b)

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f p = case p of (x, y) -> f x y

snd :: (t, t1) -> t1
snd (_,x) = x

fst :: (t, t1) -> t
fst (x,_) = x

find :: (a -> Bool) -> [a] -> Maybe a
find p (x:xs) = if p x then Just x else find p xs
find _ [] = Nothing

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

notElem :: Eq a => a -> [a] -> Bool
notElem x ys = not (elem x ys)

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
unlines = intercalate "\n"

words :: String -> [String]
words str = words' (dropWhile isSpace str)
  where words' []  = []
        words' s = case break isSpace s of (a,b) -> a : words b
        isSpace c  = c `elem` " \t\r\n\f\v"

unwords :: [String] -> String
unwords = intercalate " "

flip :: (t1 -> t2 -> t) -> t2 -> t1 -> t
flip f x y = f y x

maybe :: t -> (t1 -> t) -> Maybe t1 -> t
maybe m _ Nothing = m
maybe _ f (Just x) = f x

(.) :: (t1 -> t) -> (t2 -> t1) -> t2 -> t
(f . g) x = f (g x)
infixr 9 .

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

maximum :: (Num a, Foreign a) => [a] -> a
maximum [] = error "maximum: empty list"
maximum xs = foldl1 max xs

minimum :: (Num a, Foreign a) => [a] -> a
minimum [] = error "minimum: empty list"
minimum xs = foldl1 min xs

product :: Num a => [a] -> a
product [] = error "product: empty list"
product xs = foldl (*) 1 xs

sum :: Num a => [a] -> a
sum [] = error "sum: empty list"
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

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 _ []     = []
scanr1 _ [x]    = [x]
scanr1 f (x:xs) = case scanr1 f xs of (h:t) -> f x h : h : t

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

rem :: Int -> Int -> Int
rem x y = if y == 0 then error "Division by zero" else rem' x y
infixl 7 `rem`

rem' :: Int -> Int -> Int
rem' = ffi "%1 %% %2"

quot :: Int -> Int -> Int
quot x y = if y == 0 then error "Division by zero" else quot' x y
infixl 7 `quot`

quot' :: Int -> Int -> Int
quot' = ffi "~~(%1/%2)"

quotRem :: Int -> Int -> (Int, Int)
quotRem x y = (quot x y, rem x y)

div :: Int -> Int -> Int
div x y
  | x > 0 && y < 0 = quot (x-1) y - 1
  | x < 0 && y > 0 = quot (x+1) y - 1
div x y            = quot x y
infixl 7 `div`

mod :: Int -> Int -> Int
mod x y
  | x > 0 && y < 0 = rem (x-1) y + y + 1
  | x < 0 && y > 0 = rem (x+1) y + y - 1
mod x y            = rem x y
infixl 7 `mod`

divMod :: Int -> Int -> (Int, Int)
divMod x y
  | x > 0 && y < 0 = case (x-1) `quotRem` y of (q,r) -> (q-1, r+y+1)
  | x < 0 && y > 1 = case (x+1) `quotRem` y of (q,r) -> (q-1, r+y-1)
divMod x y         = quotRem x y

min :: (Num a, Foreign a) => a -> a -> a
min = ffi "Math.min(%1,%2)"

max :: (Num a, Foreign a) => a -> a -> a
max = ffi "Math.max(%1,%2)"

recip :: Double -> Double
recip x = 1 / x

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

sequence_ :: Monad m => [m a] -> m ()
sequence_ []     = return ()
sequence_ (m:ms) = m >> sequence_ ms

id :: a -> a
id x = x

asTypeOf :: a -> a -> a
asTypeOf = const

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until p f (f x)

($!) :: (a -> b) -> a -> b
f $! x = x `seq` f x
infixr 0 $!

(!!) :: [a] -> Int -> a
a !! b = if b < 0 then error "(!!): negative index" else go a b
  where go []    _ = error "(!!): index too large"
        go (h:_) 0 = h
        go (_:t) n = go t (n-1)
infixl 9 !!

head :: [a] -> a
head []    = error "head: empty list"
head (h:_) = h

tail :: [a] -> [a]
tail []    = error "tail: empty list"
tail (_:t) = t

init :: [a] -> [a]
init []    = error "init: empty list"
init [a]   = [a]
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
replicate n x = if n < 0 then error "replicate: negative length"
                         else x : replicate (n-1) x

cycle :: [a] -> [a]
cycle [] = error "cycle: empty list"
cycle xs = xs' where xs' = xs ++ xs'

take :: Int -> [a] -> [a]
take 0 _  = []
take _ [] = []
take n (x:xs) = if n < 0 then error "take: negative length"
                         else x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = if n < 0 then error "drop: negative length"
                         else drop (n-1) xs

splitAt :: Int -> [a] -> ([a], [a])
splitAt 0 xs     = ([], xs)
splitAt _ []     = ([], [])
splitAt n (x:xs) = if n < 0 then error "splitAt: negative length"
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

