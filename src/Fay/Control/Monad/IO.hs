-- | Alias of MonadIO.

module Fay.Control.Monad.IO where

import           Control.Monad.Trans

-- | Alias of liftIO, I hate typing it. Hate reading it.
io :: MonadIO m => IO a -> m a
io = liftIO
