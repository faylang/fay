{-# LANGUAGE NoImplicitPrelude #-}

module Alert where

import Language.Fay.FFI
import Language.Fay.Prelude

main :: Fay ()
main = alert "Hello, World!"

-- | Alert using window.alert.
alert :: Foreign a => a -> Fay ()
alert = foreignFay "window.alert" FayNone