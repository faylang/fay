module Fay.Types.FFI
  ( FundamentalType (..)
  , SerializeContext (..)
  ) where

import qualified Fay.Exts.NoAnnotation as N

-- | These are the data types that are serializable directly to native
-- JS data types. Strings, floating points and arrays. The others are:
-- actions in the JS monad, which are thunks that shouldn't be forced
-- when serialized but wrapped up as JS zero-arg functions, and
-- unknown types can't be converted but should at least be forced.
data FundamentalType
   -- Recursive types.
 = FunctionType [FundamentalType]
 | JsType FundamentalType
 | ListType FundamentalType
 | TupleType [FundamentalType]
 | UserDefined N.Name [FundamentalType]
 | Defined FundamentalType
 | Nullable FundamentalType
 -- Simple types.
 | DateType
 | StringType
 | DoubleType
 | IntType
 | BoolType
 | PtrType
 --  Automatically serialize this type.
 | Automatic
 -- Unknown.
 | UnknownType
   deriving (Show)

-- | The serialization context indicates whether we're currently
-- serializing some value or a particular field in a user-defined data
-- type.
data SerializeContext = SerializeAnywhere | SerializeUserArg Int
  deriving (Eq, Read, Show)
