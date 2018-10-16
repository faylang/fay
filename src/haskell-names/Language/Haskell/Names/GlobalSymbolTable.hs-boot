module Language.Haskell.Names.GlobalSymbolTable where

import Data.Data

data Table deriving Typeable
instance Eq   Table
instance Ord  Table
instance Show Table
instance Data Table

