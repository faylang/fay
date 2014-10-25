-- | A trivial mutex.

module Data.Mutex where

import Prelude
import Data.Var

data Mutex = Mutex (Var Bool)

-- | Make a new unlocked mutex.
newMutex :: Fay Mutex
newMutex = do v <- newVar False
              return (Mutex v)

-- | If a mutex is free run the action, otherwise don't.
ifMutexFree :: Mutex -> Fay () -> Fay ()
ifMutexFree (Mutex var) action = do
  locked <- get var
  if locked then return () else action

-- | Wait until the mutex is free to do something.
whenMutexFree :: Mutex -> Fay () -> Fay ()
whenMutexFree (Mutex var) cont = do
  locked <- get var
  if locked
     then do _ <- withUnsubscriber
                    (subscribe var)
                    (\unsubscribe lockedNow ->
                       if lockedNow
                          then return ()
                          else do unsubscribe ()
                                  cont)
             return ()

     else cont

-- | Lock the given mutex until I'm done with it.
lockMutex :: Mutex -> (Fay () -> Fay a) -> Fay a
lockMutex (Mutex var) cont = do
  locked <- get var
  if locked
     then error "mutex is already locked"
     else do set var True
             cont (set var False)
