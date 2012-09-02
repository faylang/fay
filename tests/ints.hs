{-# LANGUAGE NoImplicitPrelude #-}

module Ints where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main = do
  print 123

-- | Print using console.log.
print :: Int -> Fay ()
print = ffi "console.log(%1)"
