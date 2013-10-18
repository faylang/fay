{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | An example implementation of the lovely continuation monad.

module Cont where

import FFI
import Prelude

--------------------------------------------------------------------------------
-- Entry point.

-- | Main entry point.
main :: Fay ()
main = runContT demo (const (return ()))

demo :: Deferred ()
demo = case contT of
  CC return (>>=) (>>) callCC lift -> do
    lift (putStrLn "Hello!")
    sync setTimeout 500
    contents <- sync readFile "README.md"
    lift (putStrLn ("File contents is: " ++ take 10 contents ++ "..."))

--------------------------------------------------------------------------------
-- Deferred library.

-- | An example deferred monad.
type Deferred a = ContT () Fay a

-- | Set an asynchronous timeout.
setTimeout :: Int -> (() -> Fay ()) -> Fay ()
setTimeout = ffi "global.setTimeout(%2,%1)"

readFile :: String -> (String -> Fay b) -> Fay b
readFile = ffi "require('fs').readFile(%1,'utf-8',function(_,s){ %2(s); })"

sync :: (t -> (a -> Fay r) -> Fay r) -> t -> ContT r Fay a
sync m a = ContT $ \c -> m a c

--------------------------------------------------------------------------------
-- Continuation library.

-- | The continuation monad.
data ContT r m a = ContT { runContT :: (a -> m r) -> m r }
class Monad (m :: * -> *)
instance (Monad m) => Monad (ContT r m)

data CC = CC
  { cc_return :: forall a r. a -> ContT r Fay a
  , cc_bind :: forall a b r. ContT r Fay a -> (a -> ContT r Fay b) -> ContT r Fay b
  , cc_then :: forall a b r. ContT r Fay a -> ContT r Fay b -> ContT r Fay b
  , cc_callCC :: forall a b r. ((a -> ContT r Fay b) -> ContT r Fay a) -> ContT r Fay a
  , cc_lift :: forall a r. Fay a -> ContT r Fay a
  }

-- | The continuation monad module.
contT =
  let return a = ContT (\f -> f a)
      m >>= k = ContT $ \c -> runContT m (\a -> runContT (k a) c)
      m >> n = m >>= \_ -> n
      callCC f = ContT $ \c -> runContT (f (\a -> ContT $ \_ -> c a)) c
      lift m = ContT (\x -> m >>=* x)
  in CC return (>>=) (>>) callCC lift where (>>=*) = (>>=)
