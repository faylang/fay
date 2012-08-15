{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Fay.FFI where

import           Language.Fay.Types (Fay)
import           Prelude            (Bool, Char, Double, String, error)

-- | In case you want to distinguish values with a JsPtr.
data JsPtr a

-- | Contains allowed foreign function types.
class Foreign a

-- | Unit is OK.
instance Foreign ()

-- | All numbers in JS are double.
instance Foreign Double

-- | Characters are OK.
instance Foreign Char

-- | Bools are OK.
instance Foreign Bool

-- | Lists → arrays are OK.
instance Foreign a => Foreign [a]

-- | Pointers to arbitrary objects are OK.
instance Foreign (JsPtr a)

-- | JS values are foreignable.
instance Foreign a => Foreign (Fay a)

-- | Functions are foreignable.
instance (Foreign a,Foreign b) => Foreign (a -> b)

-- | Declare a foreign action.
ffi
  :: Foreign a
  => String        -- ^ The foreign value.
  -> a             -- ^ Bottom.
ffi = error "Language.Fay.FFI.foreignFay: Used foreign function not in a JS engine context."
