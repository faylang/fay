{-# LANGUAGE NamedFieldPuns #-}

-- | Customized queries of haskell-names interfaces (module exports).

module Fay.Compiler.Export where

import           Fay.Compiler.Misc
import           Fay.Compiler.QName
import qualified Fay.Exts.NoAnnotation  as N
import           Fay.Types

import qualified Data.Map               as M
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Language.Haskell.Names

-- TODO DRY export queries

-- getExportsFor :: N.ModuleName -> CompileState -> Maybe (Set N.QName)
-- getExportsFor modName cs =
--   fmap ( S.map (gname2Qname . origGName . sv_origName)
--        . (\(Symbols exports _) -> exports)
--        ) . M.lookup modName . stateInterfaces $ cs

-- getNonLocalExports :: N.ModuleName -> CompileState -> Maybe (Set N.QName)
-- getNonLocalExports modName cs = S.filter (\n -> qModName n /= Just modName) <$> getExportsFor modName cs

getNonLocalExportsWithoutNewtypes :: N.ModuleName -> CompileState -> Maybe (Set N.QName)
getNonLocalExportsWithoutNewtypes modName cs =
  fmap ( S.filter (\n -> qModName n /= Just modName)
       . S.map (gname2Qname . origGName . sv_origName)
       . S.filter (not . isNewtype)
       . (\(Symbols exports _) -> exports)
       )
       . M.lookup modName . stateInterfaces $ cs
  where
    isNewtype :: SymValueInfo OrigName -> Bool
    isNewtype s = case s of
      SymValue{}                     -> False
      SymMethod{}                    -> False
      SymSelector    { sv_typeName } -> not . isNewtypeDest . gname2Qname . origGName $ sv_typeName
      SymConstructor { sv_typeName } -> not . isNewtypeCons . gname2Qname . origGName $ sv_typeName
    isNewtypeDest :: N.QName -> Bool
    isNewtypeDest o = any (\(_,mdest,_) -> mdest == Just o) $ stateNewtypes cs
    isNewtypeCons :: N.QName -> Bool
    isNewtypeCons o = any (\(cons,_,_)  -> cons  == o     ) $ stateNewtypes cs
