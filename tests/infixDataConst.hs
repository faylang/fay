{-# LANGUAGE NoImplicitPrelude #-}

module Test where

import           Language.Fay.FFI
import           Language.Fay.Prelude


data Ty1 = Integer `InfixConst1` Integer
instance Foreign Ty1

data Ty2 = Bool `InfixConst2` Bool
instance Foreign Ty2

data Ty3 = Ty1 :=> Ty2
instance Foreign Ty3

t = (123 `InfixConst1` 123) :=> (False `InfixConst2` True)

main = print t

print :: Ty3 -> Fay ()
print = ffi "console.log(%1)"
