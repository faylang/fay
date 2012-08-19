-- | Mutable references.

{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ref where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main :: Fay ()
main = do
  ref <- newRef "Hello, World!"
  x <- readRef ref
  writeRef ref "Hai!"
  readRef ref >>= print

data Ref a
instance Show (Ref a)
instance Foreign a => Foreign (Ref a)

newRef :: Foreign a => a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref(%1)"

writeRef :: Foreign a => Ref a -> a -> Fay ()
writeRef = ffi "Fay$$writeRef(%1,%2)"

readRef :: Foreign a => Ref a -> Fay a
readRef = ffi "Fay$$readRef(%1)"

print :: String -> Fay ()
print = ffi "console.log(%1)"
