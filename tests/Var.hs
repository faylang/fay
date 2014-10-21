-- | Tests for the var types.

module Var where

import Data.Var

main =
  do v <- newVar (0 :: Int)
     subscribe v
               (\v ->
                  putStrLn ("v changed: " ++ show v))
     set v 4
     modify v (+ 5)
     s <- newSig
     subscribe s
               (\v ->
                  putStrLn ("s signalled: " ++ show v))
     set s (567 :: Int)
     r <- newRef (123 :: Int)
     v <- get r
     putStrLn ("ref: " ++ show v)
     set r (666 :: Int)
     v <- get r
     putStrLn ("ref(2): " ++ show v)
