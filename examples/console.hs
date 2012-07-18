{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Language.Fay.FFI
import Language.Fay.Prelude hiding (show)

-- | Main entry point.
main :: Fay ()
main = print "Hello, World!"

-- | Alert using console.log.
print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""
