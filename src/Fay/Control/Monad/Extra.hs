-- | Extra monadic functions.

module Fay.Control.Monad.Extra where

import           Control.Monad
import           Data.Maybe

-- | Word version of flip (>>=).
bind :: (Monad m) => (a -> m b) -> m a -> m b
bind = flip (>>=)

-- | When the value is Just.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) m = m a
whenJust Nothing  _ = return ()

-- | Wrap up a form in a Maybe.
just :: Functor m => m a -> m (Maybe a)
just = fmap Just

-- | Flip of mapMaybe.
forMaybe :: [a] -> (a -> Maybe b) -> [b]
forMaybe = flip mapMaybe

-- | Monadic version of maybe.
maybeM :: (Monad m) => a -> (a1 -> m a) -> Maybe a1 -> m a
maybeM nil cons a = maybe (return nil) cons a

-- | Do any of the (monadic) predicates match?
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p l = return . not . null =<< filterM p l
