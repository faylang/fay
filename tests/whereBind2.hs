{-# LANGUAGE NoImplicitPrelude #-}

module WhereBind2 where

import           Language.Fay.FFI
import           Language.Fay.Prelude

someFun x = fun x
  where fun x | x < 50 = "ok"
              | otherwise = "nop"

main :: Fay ()
main = do
    print (someFun 30)
    print (someFun 100)

print :: String -> Fay ()
print = ffi "console.log(%1)"
