{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Prelude
import FFI

main :: Fay ()
main = do
    ffi "console.log('hello world')" :: Fay ()
    ret <- ffi "(function () { return 1; })()" :: Fay Int
    print ret
