-- | Unsafe running of Fay actions in pure code.

module Fay.Unsafe where

import FFI

-- | Run a Fay action as a pure value.
unsafePerformFay :: Fay a -> a
unsafePerformFay = ffi "%1()"
