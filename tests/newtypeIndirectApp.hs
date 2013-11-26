module Main where

import           Prelude

newtype Parser a = Parser { runParser :: Char -> Either Char (a, Char) }

pure :: a -> Parser a
pure a = Parser $ \s -> Right (a, s)

p = Parser

pure' a = p (\s -> Right (a, s))

main :: Fay ()
main = do
  print $ runParser (pure 5) '0'
  print $ runParser (pure' 5) '0'
