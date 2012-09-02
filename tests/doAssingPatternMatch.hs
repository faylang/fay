{-# LANGUAGE NoImplicitPrelude #-}

module DoAssignPatternMatch where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main = do
  [1,2] <- return [1,2]
  print "OK."

print :: String -> Fay ()
print = ffi "console.log(%1)"
