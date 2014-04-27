-- | Re-exports of base functionality. Note that this module is just
-- used inside the compiler. It's not compiled to JavaScript.
-- Based on the base-extended package (c) 2013 Simon Meier, licensed as BSD3.
module Fay.Compiler.Prelude
  ( module Prelude       -- Partial

  -- * Control modules
  , module Control.Applicative
  , module Control.Arrow -- Partial
  , module Control.Monad
  , module Control.Monad.Error

  -- * Data modules
  , module Data.Char     -- Partial
  , module Data.Data     -- Partial
  , module Data.Either
  , module Data.Function
  , module Data.List     -- Partial
  , module Data.Maybe
  , module Data.Monoid   -- Partial
  , module Data.Ord
  , module Data.Tuple

  -- * Safe
  , module Safe

  -- * Additions
  , anyM
  , for
  , io
  , readAllFromProcess
  ) where

import           Control.Applicative
import           Control.Arrow       (first, second, (&&&), (***), (+++), (|||))
import           Control.Monad       hiding (guard)
import           Control.Monad.Error
import           Data.Char           hiding (GeneralCategory (..))
import           Data.Data           (Data (..), Typeable (..))
import           Data.Either
import           Data.Function       (on)
import           Data.List           hiding (delete)
import           Data.Maybe
import           Data.Monoid         (Monoid (..), (<>))
import           Data.Ord
import           Data.Tuple
import           Prelude             hiding (exp, mod)

import           Safe
import           System.Exit
import           System.Process

-- | Alias of liftIO, I hate typing it. Hate reading it.
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Do any of the (monadic) predicates match?
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p l = return . not . null =<< filterM p l

-- | Flip of map.
for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap

-- | Read from a process returning both std err and out.
readAllFromProcess :: FilePath -> [String] -> String -> IO (Either (String,String) (String,String))
readAllFromProcess program flags input = do
  (code,out,err) <- readProcessWithExitCode program flags input
  return $ case code of
    ExitFailure 127 -> Left ("cannot find executable " ++ program, "")
    ExitFailure _   -> Left (err, out)
    ExitSuccess     -> Right (err, out)
