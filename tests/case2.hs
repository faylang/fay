{-# LANGUAGE NoImplicitPrelude #-}

module Case2 where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main = print (case False of
               True -> "Hello!"
               False -> "Ney!")

print :: String -> Fay ()
print = ffi "console.log(%1)"
