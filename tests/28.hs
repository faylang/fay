{-# LANGUAGE NoImplicitPrelude #-}
module Test where

import Language.Fay.Prelude
import Language.Fay.FFI

print :: String -> Fay ()
print = ffi "console.log(%1)" FayNone

main :: Fay ()
main = print $ show $ fromInteger 5
