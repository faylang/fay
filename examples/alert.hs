{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Language.Fay.FFI
import Language.Fay.Prelude

main :: IO ()
main = alert "Hello, World!"

-- | Alert using window.alert.
alert :: Foreign a => a -> Fay ()
alert = foreignFay "window.alert" ""
