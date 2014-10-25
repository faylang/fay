{-# LANGUAGE NoImplicitPrelude #-}

module Debug.Trace where

import Prelude
import FFI

trace :: String -> Ptr a -> Ptr a
trace = ffi "console.log(%1),%2"

traceShow :: Automatic a -> Ptr b -> Ptr b
traceShow = ffi "console.log(%1),%2"
