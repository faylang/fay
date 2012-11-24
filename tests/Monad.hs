{-# LANGUAGE EmptyDataDecls    #-}

-- | Monads test.

import           Language.Fay.Prelude

main :: Fay ()
main = do
  123 <- return 123
  x <- return 456
  print x >> return ()
  x <- return 666 >>= \_ -> return 789
  y <- return 101112
  print x
  print y

