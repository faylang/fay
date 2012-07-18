module Control.Monad.IO where

import Control.Monad.Trans

io :: MonadIO m => IO a -> m a
io = liftIO
