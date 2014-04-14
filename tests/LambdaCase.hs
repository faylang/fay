{-# LANGUAGE LambdaCase #-}
module LambdaCase where

f :: Int -> Bool
f = \case
  2 -> True
  _ -> False

main :: Fay ()
main = do
  print (f 2)
