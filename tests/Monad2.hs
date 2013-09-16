{-# LANGUAGE EmptyDataDecls #-}

{-# LANGUAGE RankNTypes     #-}

-- | Monads test.

import           Prelude

main :: Fay ()
main = do
  -- State monad test
  return (let result = runState demo 60 in fst result ++ ": " ++ show (snd result)) >>= putStrLn

--------------------------------------------------------------------------------
-- A monad interface

data MonadI m = Monad
  (forall a. a -> m a)
  (forall a b. m a -> (a -> m b) -> m b)
  (forall a b. m a -> m b -> m b)

---------------------------------------------------------------------------------
-- State monad

-- Example

demo =
  case stateMonad of
    Monad return (>>=) (>>) -> do
      n <- get
      put (n*2)
      n <- get
      put (n+3)
      return "abc"

-- Definition

data State s a = State { runState :: s -> (a, s) }
instance Monad (State a)

stateMonad = Monad return (>>=) (>>) where
  return = \x -> State ( \st -> (x, st) )
  (>>=) = \processor processorGenerator ->
            State $ \st ->
              case runState processor st of
                (x, st') -> runState (processorGenerator x) st'
  (>>) = \a b -> a >>= \_ -> b

put newState = State $ \_ -> ((), newState)

get = State $ \st -> (st, st)
