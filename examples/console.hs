{-# LANGUAGE NoImplicitPrelude #-}

module Console where

import Language.Fay.FFI
import Language.Fay.Prelude

main = print "Hello, World!"

-- | Print using console.log.
print :: Foreign a => a -> Fay ()
print = ffi "console.log(%1)" FayNone
