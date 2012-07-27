{-# LANGUAGE NoImplicitPrelude #-}
module Test where

import Language.Fay.Prelude
import Language.Fay.FFI

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" FayNone

main :: Fay ()
main = print $ show $ fromInteger 5
