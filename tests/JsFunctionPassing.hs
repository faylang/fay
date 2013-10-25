{-# LANGUAGE EmptyDataDecls #-}

import           FFI
import           Prelude

data Func

makeFunc0 :: Int -> Func
makeFunc0 = ffi "function() {return %1;}"

callFunc0 :: Func -> Fay Int
callFunc0 = ffi "%1()"

makeFunc1 :: Int -> Func
makeFunc1 = ffi "function(x) {return %1;}"

callFunc1 :: Func -> Fay Int
callFunc1 = ffi "%1(1)"

main = do
  callFunc0 (makeFunc0 1) >>= print
  callFunc1 (makeFunc1 2) >>= print
