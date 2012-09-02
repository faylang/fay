{-# LANGUAGE NoImplicitPrelude #-}

import Language.Fay.FFI
import Language.Fay.Prelude


data Ty = Integer `InfixConst1` Integer
        | Bool `InfixConst2` Integer
    deriving Show

t1 = 123 `InfixConst1` 123
t2 = False `InfixConst2` 123

main = do
  print $ show t1
  print $ show t2

print :: String -> Fay ()
print = ffi "console.log(%1)"
