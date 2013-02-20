-- | Mutable references.

{-# LANGUAGE EmptyDataDecls    #-}


module Ref where

import           FFI
import           Prelude

main :: Fay ()
main = do
  ref <- newRef "Hello, World!"
  x <- readRef ref
  writeRef ref "Hai!"
  readRef ref >>= print

data Ref a
instance Show (Ref a)

newRef :: a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref(%1)"

writeRef :: Ref a -> a -> Fay ()
writeRef = ffi "Fay$$writeRef(%1,%2)"

readRef :: Ref a -> Fay a
readRef = ffi "Fay$$readRef(%1)"
