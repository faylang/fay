-- Generate Show value.
--
-- $ fay examples/data.hs
-- $ node examples/data.js
-- (Foo { x = 123, y = "abc", z = (Bar) })



module Data where

import           Language.Fay.FFI
import           Language.Fay.Prelude

data Foo = Foo { x :: Double, y :: String, z :: Foo } | Bar
  deriving (Show)
instance Foreign Foo

main = print (show (Foo 123 "abc" Bar))
