module Foo (main, x) where

import           FFI
import           Prelude

import           ReExportGlobally.A (x)

main :: Fay ()
main = ffi "console.log(Foo.x)"
