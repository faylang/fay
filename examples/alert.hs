{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Language.Fay.FFI
import Language.Fay.Prelude hiding (show)

-- | Main entry point.
main :: Fay ()
main = alert "Hello, World!"

-- | Alert using window.alert.
alert :: Foreign a => a -> Fay ()
alert = foreignFay "window.alert" ""
