{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Fay.FFI
  (Fay
  ,Foreign
  ,Nullable (..)
  ,Defined (..)
  ,Ptr
  ,Automatic
  ,ffi)
  where

import           Language.Fay.Types
import           Prelude            (Bool, Char, Double, Int, Maybe, String,
                                     error)

-- | Contains allowed foreign function types.
class Foreign a

-- | Unit is OK.
instance Foreign ()

-- | All numbers in JS are double.
instance Foreign Double

-- | Some numbers in JS are int.
instance Foreign Int

-- | Characters are OK.
instance Foreign Char

-- | Bools are OK.
instance Foreign Bool

-- | Lists → arrays are OK.
instance Foreign a => Foreign [a]

-- | Tuples → arrays are OK.
instance (Foreign a, Foreign b) => Foreign (a,b)
instance (Foreign a, Foreign b, Foreign c) => Foreign (a,b,c)
instance (Foreign a, Foreign b, Foreign c, Foreign d) => Foreign (a,b,c,d)
instance (Foreign a, Foreign b, Foreign c, Foreign d,
          Foreign e) => Foreign (a,b,c,d,e)
instance (Foreign a, Foreign b, Foreign c, Foreign d,
          Foreign e, Foreign f) => Foreign (a,b,c,d,e,f)
instance (Foreign a, Foreign b, Foreign c, Foreign d,
          Foreign e, Foreign f, Foreign g) => Foreign (a,b,c,d,e,f,g)

-- | JS values are foreignable.
instance Foreign a => Foreign (Fay a)

-- | Functions are foreignable.
instance (Foreign a,Foreign b) => Foreign (a -> b)

-- | Maybes are pretty common.
instance Foreign a => Foreign (Maybe a)

-- | Values that may be null
--  Nullable x decodes to x, Null decodes to null.
data Nullable a = Nullable a | Null
instance Foreign a => Foreign (Nullable a)

-- | Values that may be undefined
-- Defined x encodes to x, Undefined decodes to undefined.
-- An undefined property in a record will be removed when encoding.
data Defined a = Defined a | Undefined
instance Foreign a => Foreign (Defined a)

-- | Do not serialize the specified type. This is useful for, e.g.
--
-- > foo :: String -> String
-- > foo = ffi "%1"
--
-- This would normally serialize and unserialize the string, for no
-- reason, in this case. Instead:
--
-- > foo :: Ptr String -> Ptr String
--
-- Will just give an identity function.
type Ptr a = a

-- | The opposite of "Ptr". Serialize the specified polymorphic type.
--
-- > foo :: Automatic a -> String
--
type Automatic a = a

-- | Declare a foreign action.
ffi
  :: Foreign a
  => String        -- ^ The foreign value.
  -> a             -- ^ Bottom.
ffi = error "Language.Fay.FFI.foreignFay: Used foreign function not in a JS engine context."
