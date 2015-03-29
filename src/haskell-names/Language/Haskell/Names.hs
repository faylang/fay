module Language.Haskell.Names
  (
  -- * Core functions
    annotateModule
  , getInterfaces
  -- * Types
  , SymValueInfo(..)
  , Symbols(..)
  , Scoped(..)
  , NameInfo(..)
  , GName(..)
  , OrigName(..)
  , HasOrigName(..)
  ) where

import Language.Haskell.Names.Types (Symbols (..), SymValueInfo (..), Scoped (..), NameInfo (..), GName (..), OrigName (..), HasOrigName (..))
import Language.Haskell.Names.Recursive (getInterfaces, annotateModule)
