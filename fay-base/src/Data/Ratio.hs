{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -w #-}

module Data.Ratio where

import Data.Data
import Prelude
import qualified "base" Prelude as Base

--------------------------------------------------------------------------------
-- Rational

data Rational = Ratio Int Int
#ifndef FAY
    deriving Typeable
#endif

instance Base.Show Rational
instance Data Rational
#ifdef FAY
instance Typeable Rational
#endif

(%) :: Int -> Int -> Rational
x % y = reduce (x * signum y) (abs y)
  where reduce :: Int -> Int -> Rational
        reduce x y | y == 0    = error "can't devide by zero"
                   | otherwise = let d = gcd x y
                                 in Ratio (x `quot` d) (y `quot` d)

numerator, denominator :: Rational -> Int
numerator (Ratio n _) = n
denominator (Ratio _ d) = d
