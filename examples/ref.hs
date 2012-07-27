-- | Mutable references.

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Ref where

import Language.Fay.FFI
import Language.Fay.Prelude

main :: Fay ()
main = do
  ref <- newRef "Hello, World!"
  readRef ref >>= print
  writeRef ref "Hai!"
  readRef ref >>= print

print :: String -> Fay ()
print = foreignFay "console.log" ""

data Ref a
instance Foreign a => Foreign (Ref a)

newRef :: Foreign a => a -> Fay (Ref a)
newRef = foreignFay "new Fay$$Ref" FayNone

writeRef :: Foreign a => Ref a -> a -> Fay ()
writeRef = foreignFay "Fay$$writeRef" FayNone

readRef :: Foreign a => Ref a -> Fay a
readRef = foreignFay "Fay$$readRef" FayNone
