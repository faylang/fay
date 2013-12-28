module Foo (main, x) where

import           FFI

import           ReExportGlobally.A (x)

main :: Fay ()
main = ffi "console.log(Foo.x)"
