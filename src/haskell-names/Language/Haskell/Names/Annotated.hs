-- This module uses the open recursion interface
-- ("Language.Haskell.Names.Open") to annotate the AST with binding
-- information.
{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Language.Haskell.Names.Annotated
  ( Scoped (..)
  , NameInfo (..)
  , annotate
  ) where

import           Fay.Compiler.Prelude
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import qualified Language.Haskell.Names.LocalSymbolTable  as Local
import           Language.Haskell.Names.Open.Base
import           Language.Haskell.Names.Open.Instances    ()
import           Language.Haskell.Names.RecordWildcards
import           Language.Haskell.Names.Types

import           Data.Lens.Light
import           Data.Proxy
import           Language.Haskell.Exts
import           Data.Typeable ( eqT, (:~:)(Refl) )

annotate
  :: forall a l .
     (Resolvable (a (Scoped l)), Functor a, Typeable l)
  => Scope -> a l -> a (Scoped l)
annotate sc = annotateRec (Proxy :: Proxy l) sc . fmap (Scoped None)

annotateRec
  :: forall a l .
     (Typeable l, Resolvable a)
  => Proxy l -> Scope -> a -> a
annotateRec _ sc a = go sc a where
  go :: forall a . Resolvable a => Scope -> a -> a
  go sc a
    | ReferenceV <- getL nameCtx sc
    , Just (Refl :: QName (Scoped l) :~: a) <- eqT
      = lookupValue (fmap sLoc a) sc <$ a
    | ReferenceT <- getL nameCtx sc
    , Just (Refl :: QName (Scoped l) :~: a) <- eqT
      = lookupType (fmap sLoc a) sc <$ a
    | BindingV <- getL nameCtx sc
    , Just (Refl :: Name (Scoped l) :~: a) <- eqT
      = Scoped ValueBinder (sLoc . ann $ a) <$ a
    | BindingT <- getL nameCtx sc
    , Just (Refl :: Name (Scoped l) :~: a) <- eqT
      = Scoped TypeBinder (sLoc . ann $ a) <$ a
    | Just (Refl :: FieldUpdate (Scoped l) :~: a) <- eqT
      = case a of
          FieldPun l n -> FieldPun l (lookupValue (sLoc <$> n) sc <$ n)
          FieldWildcard l ->
            let
              namesUnres = sc ^. wcNames
              resolve n =
                let Scoped info _ = lookupValue (sLoc l <$ UnQual () n) sc
                in info
              namesRes =
                map
                  (\f -> (wcFieldOrigName f, resolve $ wcFieldName f))
                  namesUnres
            in FieldWildcard $ Scoped (RecExpWildcard namesRes) (sLoc l)
          _ -> rmap go sc a
    | Just (Refl :: PatField (Scoped l) :~: a) <- eqT
    , PFieldWildcard l <- a
      = PFieldWildcard $
          Scoped
            (RecPatWildcard $ map wcFieldOrigName $ sc ^. wcNames)
            (sLoc l)
    | otherwise
      = rmap go sc a

lookupValue :: QName l -> Scope -> Scoped l
lookupValue qn sc = Scoped nameInfo (ann qn)
  where
    nameInfo =
      case Local.lookupValue qn $ getL lTable sc of
        Right r -> LocalValue r
        _ ->
          case Global.lookupValue qn $ getL gTable sc of
            Global.Result r -> GlobalValue r
            Global.Error e -> ScopeError e
            Global.Special -> None

lookupType :: QName l -> Scope -> Scoped l
lookupType qn sc = Scoped nameInfo (ann qn)
  where
    nameInfo =
      case Global.lookupType qn $ getL gTable sc of
        Global.Result r -> GlobalType r
        Global.Error e -> ScopeError e
        Global.Special -> None
