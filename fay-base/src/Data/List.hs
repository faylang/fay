{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.List where

import Prelude
import Data.Maybe

-- | The 'isPrefixOf' function takes two lists and returns 'True'
-- iff the first list is a prefix of the second.
isPrefixOf              :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _         =  True
isPrefixOf _  []        =  False
isPrefixOf (x:xs) (y:ys)=  x == y && isPrefixOf xs ys

-- | The 'isSuffixOf' function takes two lists and returns 'True'
-- iff the first list is a suffix of the second.
-- Both lists must be finite.
isSuffixOf              :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf x y          =  reverse x `isPrefixOf` reverse y

-- | The 'stripPrefix' function drops the given prefix from a list.
-- It returns 'Nothing' if the list did not start with the prefix
-- given, or 'Just' the list after the prefix, if it does.
--
-- > stripPrefix "foo" "foobar" == Just "bar"
-- > stripPrefix "foo" "foo" == Just ""
-- > stripPrefix "foo" "barfoo" == Nothing
-- > stripPrefix "foo" "barfoobaz" == Nothing
stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys)
 | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

-- | Like 'stripPrefix', but drops the given suffix from the end.
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix x y = onJust reverse $ reverse x `stripPrefix` reverse y

-- | Split lists at delimiter specified by a condition
--   Drops empty groups (similar to `words`)
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p s = case dropWhile p s of
  [] -> []
  s' -> case break p s' of
    (w, s'') -> w : splitWhen p s''

-- | Split lists at the specified delimiter
--   Drops empty groups (similar to `words`)
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c = splitWhen (==c)

-- | The 'partition' function takes a predicate a list and returns
-- the pair of lists of elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p xs == (filter p xs, filter (not . p) xs)

partition :: (a -> Bool) -> [a] -> ([a],[a])
partition p xs = (filter p xs, filter (not . p) xs)
{-
-- Fay doesn't support irrefutable patterns
partition :: (a -> Bool) -> [a] -> ([a],[a])
partition p = foldr (select p) ([],[])
  where
    select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
    select p x ~(ts,fs) | p x       = (x:ts,fs)
                        | otherwise = (ts, x:fs)
-}

-- | The 'inits' function returns all initial segments of the argument,
-- shortest first.  For example,
--
-- > inits "abc" == ["","a","ab","abc"]
--
-- Note that 'inits' has the following strictness property:
-- @inits _|_ = [] : _|_@
inits                   :: [a] -> [[a]]
inits xs                =  [] : case xs of
                                  []      -> []
                                  x : xs' -> map (x :) (inits xs')

-- This one /isn't/ from Data.List
groupSortBy :: (a -> a -> Ordering) -> [a] -> [[a]]
groupSortBy f = groupBy (\x y -> f x y == EQ) . sortBy f

-- | Classic group by.
groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []           =  []
groupBy eq (x:xs)       =  case span (eq x) xs of
  (ys,zs) -> (x:ys) : groupBy eq zs

-- | Belongs in Control.Monad, right?
findM :: (a -> Fay (Maybe b)) -> [a] -> Fay (Maybe b)
findM _ [] = return Nothing
findM f (x:xs) = do
  b <- f x
  case b of
    Nothing -> findM f xs
    Just _ -> return b
