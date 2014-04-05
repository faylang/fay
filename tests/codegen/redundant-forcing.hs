-- | Strip out redundant forcing.
--
-- So:
--
-- _(print)
--
-- is redundant. We already know print is normal form.
--
-- The output for this is currently overshadowed by uncurrying, but if
-- there is a regression in one, there'll be a regression in the
-- other. So that's okay. The point here is partly just to be explicit
-- about the optimizations that are occuring.

module Main where

main = print (foo ())

foo a = ()
