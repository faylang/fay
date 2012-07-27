{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Fay.FFI where

import Language.Fay.Types (Fay,FayReturnType(..))
import Prelude (Bool,String,Double,Char,error)

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
foreignFay
  :: Foreign a
  => String        -- ^ The foreign function name.
  -> FayReturnType -- ^ JS return type.
  -> a             -- ^ Bottom.
foreignFay = error "Language.Fay.FFI.foreignFay: Used foreign function not in a JS engine context."

-- | Declare a foreign function.
foreignPure
  :: Foreign a
  => String        -- ^ The foreign function name.
  -> FayReturnType -- ^ JS return type.
  -> a             -- ^ Bottom.
foreignPure = error "Language.Fay.FFI.foreign: Used foreign function not in a JS engine context."

-- | Declare a foreign action.
foreignMethodFay
  :: Foreign a
  => String         -- ^ The foreign function name.
  -> FayReturnType  -- ^ JS return type.
  -> a              -- ^ Bottom.
foreignMethodFay = error "Language.Fay.FFI.foreignMethodFay: Used foreign function not in a JS engine context."

-- | Declare a foreign function.
foreignMethod
  :: Foreign a
  => String        -- ^ The foreign function name.
  -> FayReturnType -- ^ JS return type.
  -> a             -- ^ Bottom.
foreignMethod = error "Language.Fay.FFI.foreignMethod: Used foreign function not in a JS engine context."
