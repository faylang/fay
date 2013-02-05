module NewtypeImport_Export where

import FFI
import Prelude

newtype MyInteger = MyInteger Int

data Foo = Bar { bar :: Double }
newtype Baz = Baz { unwrapBaz :: Foo }

getBaz :: Fay Baz
getBaz = ffi "{ instance: 'Bar', bar: 1 }"
