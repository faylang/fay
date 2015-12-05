{-# OPTIONS -fno-warn-name-shadowing #-}
module Language.Haskell.Names.ScopeUtils
  ( computeSymbolTable
  , noScope
  , none
  , resolveCNames
  , scopeError
  , sv_parent
  ) where

import           Fay.Compiler.Prelude
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import           Language.Haskell.Names.SyntaxUtils
import           Language.Haskell.Names.Types

import           Data.Lens.Light
import qualified Data.Set                                 as Set
import           Language.Haskell.Exts.Annotated

scopeError :: Functor f => Error l -> f l -> f (Scoped l)
scopeError e f = Scoped (ScopeError e) <$> f

none :: l -> Scoped l
none = Scoped None

noScope :: (Annotated a) => a l -> a (Scoped l)
noScope = fmap none

sv_parent :: SymValueInfo n -> Maybe n
sv_parent (SymSelector { sv_typeName = n }) = Just n
sv_parent (SymConstructor { sv_typeName = n }) = Just n
sv_parent (SymMethod { sv_className = n }) = Just n
sv_parent _ = Nothing

computeSymbolTable
  :: Bool
    -- ^ If 'True' (\"qualified\"), then only the qualified names are
    -- inserted.
    --
    -- If 'False', then both qualified and unqualified names are insterted.
  -> ModuleName l
  -> Symbols
  -> Global.Table
computeSymbolTable qual (ModuleName _ mod) syms =
  Global.fromLists $
    if qual
      then renamed
      else renamed <> unqualified
  where
    vs = Set.toList $ syms^.valSyms
    ts = Set.toList $ syms^.tySyms
    renamed = renameSyms mod
    unqualified = renameSyms ""
    renameSyms mod = (map (rename mod) vs, map (rename mod) ts)
    rename :: HasOrigName i => ModuleNameS -> i OrigName -> (GName, i OrigName)
    rename m v = ((origGName . origName $ v) { gModule = m }, v)

resolveCName
  :: Symbols
  -> OrigName
  -> (CName l -> Error l) -- ^ error for "not found" condition
  -> CName l
  -> (CName (Scoped l), Symbols)
resolveCName syms parent notFound cn =
  let
    vs =
      [ info
      | info <- Set.toList $ syms^.valSyms
      , let name = gName . origGName $ sv_origName info
      , nameToString (unCName cn) == name
      , Just p <- return $ sv_parent info
      , p == parent
      ]
  in
    case vs of
      [] -> (scopeError (notFound cn) cn, mempty)
      [i] -> (Scoped (GlobalValue i) <$> cn, mkVal i)
      _ -> (scopeError (EInternal "resolveCName") cn, mempty)

resolveCNames
  :: Symbols
  -> OrigName
  -> (CName l -> Error l) -- ^ error for "not found" condition
  -> [CName l]
  -> ([CName (Scoped l)], Symbols)
resolveCNames syms orig notFound =
  second mconcat . unzip . map (resolveCName syms orig notFound)
