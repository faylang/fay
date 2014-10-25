-- | Functions

module Data.Function where

import Prelude

-- | (*) `on` f = \x y -> f x * f y.
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = f (g x) (g y)

-- | The \"f\" is for \"Fay\", not \"Functor\" ;)
fmap :: (a -> b) -> Fay a -> Fay b
fmap f a = a >>= return . f

-- | See '<*>'.
ap :: Fay (a -> b) -> Fay a -> Fay b
ap m g = do f <- m
            x <- g
            return (f x)

-- | A la Control.Applicative.
(<*>) :: Fay (a -> b) -> Fay a -> Fay b
(<*>) = ap
