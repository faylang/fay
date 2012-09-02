{-# LANGUAGE NoImplicitPrelude #-}

import Language.Fay.FFI
import Language.Fay.Prelude


data Ty1 = Integer `InfixConst1` Integer
    deriving Show

data Ty2 = Bool `InfixConst2` Bool
    deriving Show

data Ty3 = Ty1 :=> Ty2

t = (123 `InfixConst1` 123) :=> (False `InfixConst2` True)

main = do
  print (show t)

print :: String -> Fay ()
print = ffi "console.log(%1)"
