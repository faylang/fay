{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Fay.Compiler.ModuleT
  (
  -- * Module monad
  -- | When you need to resolve modules, you work in a 'ModuleT' monad (or
  -- another monad that is an instance of 'MonadModule') and use the
  -- 'getModuleInfo' function.
  --
  -- It finds an installed module by its name and reads (and caches) its
  -- info from the info file. Then you run a 'ModuleT' monadic action
  -- using 'evalModuleT' or 'runModuleT'.
  --
  -- To run a 'ModuleT' action you'll also need to provide the set of
  -- packages (represented by their 'InstalledPackageInfo') in which to
  -- search for modules. You can get such a set from either
  -- 'getInstalledPackages' or 'readPackagesInfo', depending on your use
  -- case.
    ModuleT
  , getModuleInfo
  , runModuleT
  , MonadModule (..)
  -- * Module names
  , ModName (..)
  ) where

import           Fay.Compiler.Prelude

import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Char            as Char (isAlphaNum, isUpper)
import qualified Data.Map             as Map

-- ModuleName extracted from Cabal, (c) 2008 Duncan Coutts, Licensed as BSD3
newtype ModuleName = ModuleName [String]
  deriving (Eq, Ord, Show)

fromString :: String -> ModuleName
fromString string
  | all validModuleComponent components' = ModuleName components'
  | otherwise                            = error $ "ModuleName.fromString: invalid module name " ++ show string

  where
    components' = split string

    split cs = case break (=='.') cs of
      (chunk,[])     -> chunk : []
      (chunk,_:rest) -> chunk : split rest

    validModuleComponent :: String -> Bool
    validModuleComponent []     = False
    validModuleComponent (c:cs) = Char.isUpper c
                           && all validModuleChar cs

    validModuleChar :: Char -> Bool
    validModuleChar c = Char.isAlphaNum c || c == '_' || c == '\''


-- | This class defines the interface that is used by 'getModuleInfo', so
-- that you can use it in monads other than 'ModuleT'.
--
-- You don't typically have to define your own instances of this class, but
-- here are a couple of cases when you might:
--
-- * A pure (non-'MonadIO') mockup module monad for testing purposes
--
-- * A transformer over 'ModuleT'
--
-- * You need a more complex way to retrieve the module info
class Monad m => MonadModule m where
  -- | The type of module info
  type ModuleInfo m
  lookupInCache :: ModName n => n -> m (Maybe (ModuleInfo m))
  insertInCache :: ModName n => n -> ModuleInfo m -> m ()

  -- | Read the module info, given a list of search paths and the module
  -- name
  readModuleInfo :: ModName n => [FilePath] -> n -> m (ModuleInfo m)

-- | Different libraries (Cabal, haskell-src-exts, ...) use different types
-- to represent module names. Hence this class.
class ModName n where
  modToString :: n -> String

instance ModName String where
  modToString = id

-- | Convert module name from arbitrary representation to Cabal's one
convertModuleName :: ModName n => n -> ModuleName
convertModuleName = fromString . modToString

-- | Tries to find the module in the current set of packages, then find the
-- module's info file, and reads and caches its contents.
--
-- Returns 'Nothing' if the module could not be found in the current set of
-- packages. If the module is found, but something else goes wrong (e.g.
-- there's no info file for it), an exception is thrown.
getModuleInfo :: (MonadModule m, ModName n) => n -> m (Maybe (ModuleInfo m))
getModuleInfo = lookupInCache

-- | A standard module monad transformer.
--
-- @i@ is the type of module info, @m@ is the underlying monad.
newtype ModuleT i m a =
  ModuleT (
    (StateT (Map.Map ModuleName i)
            (ReaderT ([FilePath] -> ModuleName -> m i) m) a))
  deriving (Functor, Applicative, Monad)

instance MonadTrans (ModuleT i) where
  lift = ModuleT . lift . lift

instance MonadIO m => MonadIO (ModuleT i m) where
  liftIO = ModuleT . liftIO

instance (Functor m, Monad m) => MonadModule (ModuleT i m) where
  type ModuleInfo (ModuleT i m) = i
  lookupInCache n = ModuleT $ Map.lookup (convertModuleName n) <$> get
  insertInCache n i = ModuleT $ modify $ Map.insert (convertModuleName n) i
  readModuleInfo dirs mod' =
    lift =<< ModuleT ask <*> pure dirs <*> pure (convertModuleName mod')

-- | Run a 'ModuleT' action
runModuleT
  :: (Monad m, Monoid i)
  => ModuleT i m a -- ^ the monadic action to run
  -> m (a, Map.Map ModuleName i)
  -- ^ return value, plus all cached module infos (that is, the initial set
  -- plus all infos that have been read by the action itself)
runModuleT (ModuleT a) =
  runReaderT (runStateT a Map.empty) (\_ _ -> return mempty)
