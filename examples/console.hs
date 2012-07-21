{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Language.Fay.FFI
import Language.Fay.Prelude

main = print "Hello, World!"

-- | Print using console.log.
print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""
