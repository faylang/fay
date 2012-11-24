{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Fay.FFI
  (Fay
  ,Foreign
  ,ffi)
  where

import           Language.Fay.Types (Fay)
import           Prelude            (Bool, Char, Double, String, Int, Maybe, error)

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

-- | Declare a foreign action.
ffi
  :: Foreign a
  => String        -- ^ The foreign value.
  -> a             -- ^ Bottom.
ffi = error "Language.Fay.FFI.foreignFay: Used foreign function not in a JS engine context."
