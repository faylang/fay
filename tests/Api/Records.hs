module Api.Records where

data R = R Int Int | S { x :: Int, y :: Int }
data T = Int :+ Int
