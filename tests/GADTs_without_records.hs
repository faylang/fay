{-# LANGUAGE GADTs #-}

module GADTs_without_records where

import Prelude

data Expr a where
  I :: Int -> Expr Int
  Plus :: Expr Int -> Expr Int -> Expr Int
  B :: Bool -> Expr Bool
  IfThenElse :: Expr Bool -> Expr a -> Expr a -> Expr a
  
false :: Expr Bool
false = B False

true :: Expr Bool
true = B True

eval :: Expr a -> a
eval (I x) = x
eval (Plus a1 a2) = eval a1 + eval a2
eval (B x) = x
eval (IfThenElse p e1 e2) = case eval p of
  True -> eval e1
  False -> eval e2
  
n5 = I 5
n2 = I 2

expr1 = Plus n5 n2

expr2 = IfThenElse true expr1 n5
expr3 = IfThenElse false expr1 n5

main = do
  when (eval n5 == 5) $ putStrLn "Expected 5: Ok"
  when (eval n2 == 2) $ putStrLn "Expected 2: Ok"
  when (eval expr1 == 7) $ putStrLn "Expected 7: Ok"
  when (eval expr2 == 7) $ putStrLn "Expected 7: Ok"
  when (eval expr3 == 5) $ putStrLn "Expected 5: Ok"
