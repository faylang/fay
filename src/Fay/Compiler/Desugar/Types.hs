{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- | The transformer stack used during desugaring.

module Fay.Compiler.Desugar.Types
  ( DesugarReader (..)
  , Desugar
  , runDesugar
  ) where

import           Fay.Types            (CompileError (..))

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
#endif
import           Control.Monad.Except
import           Control.Monad.Reader

data DesugarReader l = DesugarReader
  { readerNameDepth     :: Int
  , readerNoInfo        :: l
  , readerTmpNamePrefix :: String
  }

newtype Desugar l a = Desugar
  { unDesugar :: (ReaderT (DesugarReader l)
                       (ExceptT CompileError IO))
                       a
  } deriving ( MonadReader (DesugarReader l)
             , MonadError CompileError
             , MonadIO
             , Monad
             , Functor
             , Applicative
             )

runDesugar :: String -> l -> Desugar l a -> IO (Either CompileError a)
runDesugar tmpNamePrefix emptyAnnotation m =
    runExceptT (runReaderT (unDesugar m) (DesugarReader 0 emptyAnnotation tmpNamePrefix))
