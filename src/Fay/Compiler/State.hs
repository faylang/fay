{-# LANGUAGE NamedFieldPuns #-}

-- | Pure functions for working with CompileState

module Fay.Compiler.State where

import           Fay.Compiler.Misc
import           Fay.Compiler.QName
import qualified Fay.Exts.NoAnnotation  as N
import           Fay.Types

import qualified Data.Map               as M
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Language.Haskell.Names

-- | Get all non local identifiers that should be exported in the JS module scope.
getNonLocalExportsWithoutNewtypes :: N.ModuleName -> CompileState -> Maybe (Set N.QName)
getNonLocalExportsWithoutNewtypes modName cs =
  fmap ( S.filter (not . isLocal)
       . S.map (origName2QName . sv_origName)
       . S.filter (not . (`isNewtype` cs))
       . (\(Symbols exports _) -> exports)
       )
       . M.lookup modName . stateInterfaces $ cs
  where
   isLocal = (Just modName ==) . qModName

getLocalExportsWithoutNewtypes :: N.ModuleName -> CompileState -> Maybe (Set N.QName)
getLocalExportsWithoutNewtypes modName cs =
  fmap ( S.filter isLocal
       . S.map (origName2QName . sv_origName)
       . S.filter (not . (`isNewtype` cs))
       . (\(Symbols exports _) -> exports)
       )
       . M.lookup modName . stateInterfaces $ cs
  where
   isLocal = (Just modName ==) . qModName

-- | Is this *resolved* name a new type constructor or destructor?
isNewtype :: SymValueInfo OrigName -> CompileState -> Bool
isNewtype s cs = case s of
  SymValue{}                     -> False
  SymMethod{}                    -> False
  SymSelector    { sv_typeName } -> not . (`isNewtypeDest` cs) . origName2QName $ sv_typeName
  SymConstructor { sv_typeName } -> not . (`isNewtypeCons` cs) . origName2QName $ sv_typeName

-- | Is this *resolved* name a new type destructor?
isNewtypeDest :: N.QName -> CompileState -> Bool
isNewtypeDest o = any (\(_,mdest,_) -> mdest == Just o) . stateNewtypes

-- | Is this *resolved* name a new type constructor?
isNewtypeCons :: N.QName -> CompileState -> Bool
isNewtypeCons o = any (\(cons,_,_) -> cons  == o) . stateNewtypes

-- | Add a ModulePath to CompileState, meaning it has been printed.
addModulePath :: ModulePath -> CompileState -> CompileState
addModulePath mp cs = cs { stateJsModulePaths = mp `S.insert` stateJsModulePaths cs }

-- | Has this ModulePath been added/printed?
addedModulePath :: ModulePath -> CompileState -> Bool
addedModulePath mp CompileState { stateJsModulePaths } = mp `S.member` stateJsModulePaths


findTypeSig :: N.QName -> CompileState -> Maybe N.Type
findTypeSig n  = M.lookup n . stateTypeSigs
