-- | Unsafe coerce.

module Unsafe.Coerce where

import FFI

unsafeCoerce :: a -> b
unsafeCoerce = ffi "%1"
