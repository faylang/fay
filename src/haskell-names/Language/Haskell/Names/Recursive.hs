{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Language.Haskell.Names.Recursive
  ( computeInterfaces
  , getInterfaces
  , annotateModule
  ) where

import           Fay.Compiler.Prelude

import           Fay.Compiler.ModuleT
import           Language.Haskell.Names.Annotated
import           Language.Haskell.Names.Exports
import           Language.Haskell.Names.Imports
import           Language.Haskell.Names.ModuleSymbols
import           Language.Haskell.Names.Open.Base
import           Language.Haskell.Names.ScopeUtils
import           Language.Haskell.Names.SyntaxUtils
import           Language.Haskell.Names.Types

import           Data.Data                            (Data)
import           Data.Foldable
import           Data.Graph                           (flattenSCC, stronglyConnComp)
import qualified Data.Set                             as Set
import           Language.Haskell.Exts


-- | Take a set of modules and return a list of sets, where each sets for
-- a strongly connected component in the import graph.
-- The boolean determines if imports using @SOURCE@ are taken into account.
groupModules :: forall l . [Module l] -> [[Module l]]
groupModules modules =
  map flattenSCC $ stronglyConnComp $ map mkNode modules
  where
    mkNode :: Module l -> (Module l, ModuleName (), [ModuleName ()])
    mkNode m =
      ( m
      , dropAnn $ getModuleName m
      , map (dropAnn . importModule) $ getImports m
      )

-- | Annotate a module with scoping information. This assumes that all
-- module dependencies have been resolved and cached — usually you need
-- to run 'computeInterfaces' first, unless you have one module in
-- isolation.
annotateModule
  :: (MonadModule m, ModuleInfo m ~ Symbols, Data l, SrcInfo l, Eq l)
  => Language -- ^ base language
  -> [Extension] -- ^ global extensions (e.g. specified on the command line)
  -> Module l -- ^ input module
  -> m (Module (Scoped l)) -- ^ output (annotated) module
annotateModule lang exts mod@(Module lm mh os is ds) = do
  let extSet = moduleExtensions lang exts mod
  (imp, impTbl) <- processImports extSet is
  let tbl = moduleTable impTbl mod
  (exp, _syms) <- processExports tbl mod

  let
    lm' = none lm
    os' = fmap noScope os
    is' = imp
    ds' = annotate (initialScope tbl) `map` ds

    mh' = flip fmap mh $ \(ModuleHead lh n mw _me) ->
      let
        lh' = none lh
        n'  = noScope n
        mw' = fmap noScope mw
        me' = exp
      in ModuleHead lh' n' mw' me'

  return $ Module lm' mh' os' is' ds'

annotateModule _ _ _ = error "annotateModule: non-standard modules are not supported"

-- | Compute interfaces for a set of mutually recursive modules and write
-- the results to the cache. Return the set of import/export errors.
findFixPoint
  :: (Ord l, Data l, MonadModule m, ModuleInfo m ~ Symbols)
  => [(Module l, ExtensionSet)]
      -- ^ module and all extensions with which it is to be compiled.
      -- Use 'moduleExtensions' to build this list.
  -> m (Set.Set (Error l))
findFixPoint mods = go mods (map (const mempty) mods) where
  go mods syms = do
    forM_ (zip syms mods) $ \(s,(m, _)) -> insertInCache (getModuleName m) s
    (syms', errors) <- liftM unzip $ forM mods $ \(m, extSet) -> do
      (imp, impTbl) <- processImports extSet $ getImports m
      let tbl = moduleTable impTbl m
      (exp, syms) <- processExports tbl m
      return (syms, foldMap getErrors imp <> foldMap getErrors exp)
    if syms' == syms
      then return $ mconcat errors
      else go mods syms'

-- | 'computeInterfaces' takes a list of possibly recursive modules and
-- computes the interface of each module. The computed interfaces are
-- written into the @m@'s cache and are available to further computations
-- in this monad.
--
-- Returns the set of import/export errors. Note that the interfaces are
-- registered in the cache regardless of whether there are any errors, but
-- if there are errors, the interfaces may be incomplete.
computeInterfaces
  :: (MonadModule m, ModuleInfo m ~ Symbols, Data l, SrcInfo l, Ord l)
  => Language -- ^ base language
  -> [Extension] -- ^ global extensions (e.g. specified on the command line)
  -> [Module l] -- ^ input modules
  -> m (Set.Set (Error l)) -- ^ errors in export or import lists
computeInterfaces lang exts =
  liftM fold . mapM findFixPoint . map supplyExtensions . groupModules
    where
    supplyExtensions = map $ \m -> (m, moduleExtensions lang exts m)

-- | Like 'computeInterfaces', but also returns a list of interfaces, one
-- per module and in the same order
getInterfaces
  :: (MonadModule m, ModuleInfo m ~ Symbols, Data l, SrcInfo l, Ord l)
  => Language -- ^ base language
  -> [Extension] -- ^ global extensions (e.g. specified on the command line)
  -> [Module l] -- ^ input modules
  -> m ([Symbols], Set.Set (Error l)) -- ^ output modules, and errors in export or import lists
getInterfaces lang exts mods = do
  errs <- computeInterfaces lang exts mods
  ifaces <- forM mods $ \mod ->
    let modName = getModuleName mod in
    fromMaybe (error $ msg modName) `liftM` lookupInCache modName
  return (ifaces, errs)
  where
    msg modName = "getInterfaces: module " ++ modToString modName ++ " is not in the cache"
