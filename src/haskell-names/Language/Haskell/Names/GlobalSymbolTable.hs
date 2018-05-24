{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | This module is designed to be imported qualified.
module Language.Haskell.Names.GlobalSymbolTable
  ( Table
  , empty
  , Result(..)
  , lookupValue
  , lookupType
  , fromLists
  , types
  , values
  , toSymbols
  ) where

import           Fay.Compiler.Prelude               hiding (empty)
import           Language.Haskell.Names.SyntaxUtils
import           Language.Haskell.Names.Types

import           Data.Lens.Light
import qualified Data.Map                           as Map
import qualified Data.Set                           as Set
import           Language.Haskell.Exts    as HSE
import           Data.Semigroup (Semigroup)

-- | Global symbol table — contains global names
data Table =
  Table
    (Map.Map GName (Set.Set (SymValueInfo OrigName)))
    (Map.Map GName (Set.Set (SymTypeInfo  OrigName)))
    deriving (Eq, Ord, Show, Data, Typeable)

valLens :: Lens Table (Map.Map GName (Set.Set (SymValueInfo OrigName)))
valLens = lens (\(Table vs _) -> vs) (\vs (Table _ ts) -> Table vs ts)

tyLens :: Lens Table (Map.Map GName (Set.Set (SymTypeInfo OrigName)))
tyLens = lens (\(Table _ ts) -> ts) (\ts (Table vs _) -> Table vs ts)

instance Semigroup Table where
  (Table vs1 ts1) <> (Table vs2 ts2) =
    Table (j vs1 vs2) (j ts1 ts2)
    where
      j :: (Ord i, Ord k)
        => Map.Map k (Set.Set i)
        -> Map.Map k (Set.Set i)
        -> Map.Map k (Set.Set i)
      j = Map.unionWith Set.union
instance Monoid Table where
  mempty = empty

toGName :: QName l -> GName
toGName (UnQual _ n) = GName "" (nameToString n)
toGName (Qual _ (ModuleName _ m) n) = GName m (nameToString n)
toGName (HSE.Special _ _) = error "toGName: Special"

empty :: Table
empty = Table Map.empty Map.empty

lookupL
  :: HasOrigName i
  => Lens Table (Map.Map GName (Set.Set (i OrigName)))
  -> QName l
  -> Table
  -> Result l (i OrigName)
lookupL _ (HSE.Special {}) _ =
  Language.Haskell.Names.GlobalSymbolTable.Special
lookupL lens qn tbl =
  case Set.toList <$> (Map.lookup (toGName qn) $ getL lens tbl) of
    Nothing -> Error $ ENotInScope qn
    Just [] -> Error $ ENotInScope qn
    Just [i] -> Result i
    Just is -> Error $ EAmbiguous qn (map origName is)

data Result l a
  = Result a
  | Error (Error l)
  | Special

lookupValue :: QName l -> Table -> Result l (SymValueInfo OrigName)
lookupValue = lookupL valLens

lookupType :: QName l -> Table -> Result l (SymTypeInfo OrigName)
lookupType  = lookupL tyLens

fromMaps
  :: Map.Map GName (Set.Set (SymValueInfo OrigName))
  -> Map.Map GName (Set.Set (SymTypeInfo  OrigName))
  -> Table
fromMaps = Table

fromLists
  :: ([(GName, SymValueInfo OrigName)],
      [(GName, SymTypeInfo OrigName)])
  -> Table
fromLists (vs, ts) =
  fromMaps
    (Map.fromListWith Set.union $ map (second Set.singleton) vs)
    (Map.fromListWith Set.union $ map (second Set.singleton) ts)

values :: Table -> Map.Map GName (Set.Set (SymValueInfo OrigName))
types  :: Table -> Map.Map GName (Set.Set (SymTypeInfo  OrigName))
values = getL valLens
types = getL tyLens

toSymbols :: Table -> Symbols
toSymbols tbl =
  Symbols
    (gather $ values tbl)
    (gather $ types  tbl)
  where
    gather :: Ord a => Map.Map k (Set.Set a) -> Set.Set a
    gather = Map.foldl' Set.union Set.empty
