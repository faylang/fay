{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Var
  (
  -- * Different types of variables
    Sig
  , newSig
  , Ref
  , newRef
  , Var
  , newVar

  -- * Generic operations
  , Settable
  , set
  , Gettable
  , get
  , modify
  , modifyWith
  , Subscribable
  , subscribe
  , withUnsubscriber

  -- * Specific operations
  , subscribeWithOld
  , subscribeChange
  , subscribeAndRead
  , subscribeChangeAndRead
  , subscribeExclusive
  , subscribeAndReadExclusive
  , mapVar
  , mergeVars
  , mergeVars'
  , tupleVars
  , tupleVars'
  , waitForN
  , waitFor
  , oneShot
  , holdSig

  ) where

import Data.Maybe
import FFI
import Prelude

-- | A subscribable signal.  Can have handlers subscribed to them, but doesn't
--   store a value.
data Sig a

-- | Make a new signal.
newSig :: Fay (Sig a)
newSig = ffi "new Fay$$Sig()"

-- | A mutable reference, with no subscribers.
data Ref a

-- | Make a new mutable reference.
newRef :: a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref2(%1)"

-- | A reactive variable.  Stores a value, and can have handlers subscribed to
--   changes.
data Var a

-- | Make a new reactive variable.
newVar :: a -> Fay (Var a)
newVar = ffi "new Fay$$Var(%1)"


-- | All of the variable types can be set to a value.
class Settable v
instance Settable (Ref a)
instance Settable (Sig a)
instance Settable (Var a)

-- | Write to the value (if any), and call subscribers (if any).
set :: Settable (v a) => v a -> a -> Fay ()
set = ffi "Fay$$setValue(Fay$$_(%1), %2, Fay$$_)"


-- | 'Ref' and 'Var' store their last set value.
class Gettable v
instance Gettable (Ref a)
instance Gettable (Var a)

-- | Get the value of a 'Ref' or 'Var'.
get :: Gettable (v a) => v a -> Fay a
get = ffi "Fay$$_(%1).val"

-- | Modifies the current value with a pure function.
modify :: (Settable (v a), Gettable (v a)) => v a -> (a -> a) -> Fay ()
modify v f = get v >>= set v . f

-- | Runs a 'Fay' action on the current value, and updates with the result.
modifyWith :: (Settable (v a), Gettable (v a)) => v a -> (a -> Fay a) -> Fay ()
modifyWith v f = get v >>= f >>= set v

-- | 'Sig' and 'Var' have lists of subscribers that are notified when 'set' is
--   used.
class Settable v => Subscribable v
instance Subscribable (Sig a)
instance Subscribable (Var a)

-- | Subscribe to the value of a 'Sig' or 'Var'.
--
--   The result is an unsubscribe function.
subscribe :: Subscribable (v a) => v a -> Ptr (a -> Fay void) -> Fay (() -> Fay ())
subscribe = ffi "Fay$$subscribe(Fay$$_(%1), Fay$$_(%2))"

-- | Run the same subscribing action but provide an additional
-- unsubscribe parameter to the handler.
withUnsubscriber :: ((a -> Fay ()) -> Fay (() -> Fay ()))
                 -> (((() -> Fay ()) -> a -> Fay ()) -> Fay (() -> Fay ()))
withUnsubscriber f = \g -> do
  unsubscriber <- newRef Nothing
  unsubscribe <- f $ \v -> do munsubscriber <- get unsubscriber
                              whenJust munsubscriber $ \unsubscribe -> g unsubscribe v
  set unsubscriber (Just unsubscribe)
  return unsubscribe

-- | Subscribe to a 'Var', along with the previous value.
--
--   The result is an unsubscribe function.
subscribeWithOld :: Var a -> (a -> a -> Fay ()) -> Fay (() -> Fay ())
subscribeWithOld v f = do
  o <- get v >>= newRef
  subscribe v $ \x' -> do
    x <- get o
    set o x'
    f x x'

-- | Subscribe to a 'Var', but only call handler when it actually changes.
--
--   The result is an unsubscribe function.
subscribeChange :: Eq a => Var a -> (a -> Fay ()) -> Fay (() -> Fay ())
subscribeChange v f = subscribeWithOld v $ \x x' -> when (x /= x') $ f x'

-- | Subscribe to a 'Var', and call the function on the current value.
--
--   The result is an unsubscribe function.
subscribeAndRead :: Var a -> (a -> Fay void) -> Fay (() -> Fay ())
subscribeAndRead v f = do
  x <- get v
  f x
  subscribe v f

-- | Subscribe to a 'Var', but only call handler when it actually changes, and
--   also initially on registration.
--
--   The result is an unsubscribe function.
subscribeChangeAndRead :: Eq a => Var a -> (a -> Fay ()) -> Fay (() -> Fay ())
subscribeChangeAndRead v f = do
  x <- get v
  f x
  subscribeChange v f


-- | Given a change handler, returns a function that can be used to set a
--   subscribable without invoking the handler.  This can be useful in
--   situations where the handler for a 'Var' causes an event which otherwise
--   ought to set the value of the 'Var'.  An example of this is interfacing
--   with HTML input field change events.
--
--   The 'snd' part of the result is an unsubscribe function.
subscribeExclusive :: Subscribable (v a) => v a -> (a -> Fay ()) -> Fay (a -> Fay (), () -> Fay ())
subscribeExclusive v onChange = do
  bracket <- getBracket
  unsubscribe <- subscribe v $ bracket . onChange
  return (\x -> bracket $ set v x, unsubscribe)

-- | Given a change handler, returns a function that can be used to set a var
--   without invoking the handler. The handler is called with the initial
--   value. This can be useful in situations where the handler for a 'Var'
--   causes an event which otherwise ought to set the value of the 'Var'.  An
--   example of this is interfacing with HTML input field change events.
--
--   The 'snd' part of the result is an unsubscribe function.
subscribeAndReadExclusive :: Var a -> (a -> Fay ()) -> Fay (a -> Fay (), () -> Fay ())
subscribeAndReadExclusive v onChange = do
  bracket <- getBracket
  unsubscribe <- subscribeAndRead v $ bracket . onChange
  return (\x -> bracket $ set v x, unsubscribe)

-- Utility used for 'subscribeExclusive', 'subscribeAndReadExclusive', and
-- 'mergeVars'.
getBracket :: Fay (Fay () -> Fay ())
getBracket = do
  rhandle <- newRef True
  return $ \f -> do
    handle <- get rhandle
    when handle $ do
      set rhandle False
      f
      set rhandle True

--TODO: mapVar variant that's bidirectional?
--TODO: return unsubscribe?

-- | Creates a 'Var' that updates whenever the source var is changed, applying
--   the provided function to compute the new value.
mapVar :: (a -> b) -> Var a -> Fay (Var b)
mapVar f v = do
  x <- get v
  r <- newVar (f x)
  _ <- subscribe v $ \x' -> set r $ f x'
  return r

-- | Creates a 'Var' that updates whenever one of its source vars are changed.
--   If the 2nd argument is a 'Just' value, then its used to set the source
--   vars when the variable is changed. Setting using a merged var is
--   sometimes preferred because both values are set before the subscribers
--   are called.
--
--   The 'snd' part of the result is an unsubscribe function.
mergeVars :: (a -> b -> c) -> Maybe (c -> (a, b)) -> Var a -> Var b
          -> Fay (Var c, Fay ())
mergeVars f mg va vb = do
  bracket <- getBracket
  a0 <- get va
  b0 <- get vb
  vc <- newVar (f a0 b0)
  unsubscribeA <- subscribe va $ \a -> bracket $ do
    b <- get vb
    set vc (f a b)
  unsubscribeB <- subscribe vb $ \b -> bracket $ do
    a <- get va
    set vc (f a b)
  unsubscribe <- case mg of
    Nothing -> return $ unsubscribeA () >> unsubscribeB ()
    Just g -> do
      unsubscribeC <- subscribe vc $ \c -> bracket $ case g c of
        (a, b) -> do
          -- Set variables before broadcast.
          setInternal va a
          setInternal vb b
          broadcastInternal va a
          broadcastInternal vb b
      return $ unsubscribeA () >> unsubscribeB () >> unsubscribeC ()
  return (vc, unsubscribe)

setInternal :: Var a -> a -> Fay ()
setInternal = ffi "function() { Fay$$_(%1).val = %2; }()"

broadcastInternal :: Var a -> a -> Fay ()
broadcastInternal = ffi "Fay$$broadcastInternal(Fay$$_(%1), %2, Fay$$_)"

-- | Like 'mergeVars', but discards the unsubscribe function.
mergeVars' :: (a -> b -> c) -> Maybe (c -> (a, b)) -> Var a -> Var b
           -> Fay (Var c)
mergeVars' f mg va vb = do
  result <- mergeVars f mg va vb
  case result of
    (v, _) -> return v

-- | Creates a 'Var' that updates whenever one of its source vars are changed.
--   It can also be used to set both source vars at once.
--
--   See 'mergeVars' for more information.  Note that when using nested tuples,
--   if you want all of the values to be set before broadcast, then they should
--   nest to the left.
tupleVars :: Var a -> Var b -> Fay (Var (a, b), Fay ())
tupleVars = mergeVars (\x y -> (x, y)) (Just id)

-- | Like 'tupleVars', but discards the unsubscribe function.
tupleVars' :: Var a -> Var b -> Fay (Var (a, b))
tupleVars' va vb = do
  result <- tupleVars va vb
  case result of
    (v, _) -> return v

-- | Wait for n signals on the given signaller.
waitForN :: Int -> Fay (Fay void -> Fay (),Sig ())
waitForN n = do
  sig <- newSig
  count <- newVar (0 :: Int)
  _ <- subscribe sig (const (modify count (+1)))
  return (\m -> subscribeAndRead count (\i -> when (i == n) (m >> return ())) >> return (),sig)

-- | Wait for the given predicate to be satisfied on the var and then
-- unsubscribe.
waitFor :: Var a -> (a -> Bool) -> (a -> Fay ()) -> Fay ()
waitFor v p f = do
  _ <- withUnsubscriber (subscribeAndRead v)
    $ \unsubscribe x -> when (p x) $ unsubscribe () >> f x
  return ()

-- | Make a one-shot variable subscription that immediately
-- unsubscribes after the event has triggered.
oneShot :: Subscribable (v a) => v a -> (a -> Fay ()) -> Fay ()
oneShot v f = do
  _ <- withUnsubscriber (subscribe v) $ \unsubscribe x -> unsubscribe () >> f x
  return ()

-- | Turn a sig into a var, by storing the last reported value.
holdSig :: a -> Sig a -> Fay (Var a)
holdSig initial sig = do
  v <- newVar initial
  void $ subscribe sig $ set v
  return v
