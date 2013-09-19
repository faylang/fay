{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

import           FFI
import           Prelude

data C = C { a :: Int, b :: Int, c :: Int, d :: Int }

data X = X { foo :: Int } | Y { foo :: Int }

partialMatch :: C -> Int
partialMatch C{a=x, ..} = x + d

con :: C
con = let {a=10; b=20; c=30; d=40} in C{..}

match :: X -> Int
match X{..} = foo

partialCon :: C
partialCon = let a = 11; b = 2; c = 3; d = 4 in C { a = 1, ..}

partialMatch2 c =
  let a = 100
  in case c of
       C{a=x,..} -> a

main = do
  print con
  print partialCon

  print $ match X{foo=9}
  print $ partialMatch C{a=1, b=2, c=3, d=4}
  print $ partialMatch2 $ C 1 2 3 4


  -- non exhaustive pattern match in `match`
  let y = Y{foo=6}
  print (match y)
