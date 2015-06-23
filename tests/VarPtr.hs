module VarPtr where

import Data.Var

data Record = Record Int

main = do 
  v <- newVar $ Record 5
  subscribeAndRead v $ \y -> case y of
    Record a -> putStrLn . show $ a
  set v $ Record 10
