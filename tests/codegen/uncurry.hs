-- | Uncurrying test.
--
-- So:
--
-- _(_(foo)(x))(y)
--
-- should become:
--
-- foo$uncurried(x,y)
--

module Main where

main = print (foo () ())

foo a b = ()
