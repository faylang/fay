-- | The transformer stack used during desugaring.
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Fay.Compiler.Desugar.Types
  ( DesugarReader (..)
  , Desugar
  , runDesugar
  ) where

import           Fay.Compiler.Prelude

import           Fay.Types            (CompileError (..))

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
