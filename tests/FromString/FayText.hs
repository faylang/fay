{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP                #-}
-- | Module to be shared between server and client.
--
-- This module must be valid for both GHC and Fay.
module FromString.FayText where

import           Prelude
#ifdef FAY
import           FFI
#else
import           Fay.FFI
#endif
import           Data.Data

#ifdef FAY

data Text = Text
    deriving (Show, Read, Eq, Typeable, Data)

pack :: String -> Text
pack = ffi "%1"

unpack :: Text -> String
unpack = ffi "%1"

#else

import qualified Data.Text as T

type Text = T.Text

pack :: String -> Text
pack = T.pack

unpack :: Text -> String
unpack = T.unpack

#endif

fromString :: String -> Text
fromString = pack
