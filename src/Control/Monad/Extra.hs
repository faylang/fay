module Control.Monad.Extra where

import           Data.Maybe

ig :: (Monad m) => m a -> m ()
ig m = m >> return ()

bind :: (Monad m) => (a -> m b) -> m a -> m b
bind = flip (>>=)

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) m = m a
whenJust Nothing  _ = return ()

-- | Wrap up a form in a Maybe.
just :: Functor m => m a -> m (Maybe a)
just = fmap Just

forMaybe :: [a] -> (a -> Maybe b) -> [b]
forMaybe = flip mapMaybe

maybeM :: (Monad m) => a -> (a1 -> m a) -> Maybe a1 -> m a
maybeM nil cons a = maybe (return nil) cons a
