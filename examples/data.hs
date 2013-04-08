-- Generate Show value.
--
-- $ fay examples/data.hs
-- $ node examples/data.js
-- (Foo { x = 123, y = "abc", z = (Bar) })



module Data where

import           FFI
import           Prelude

data Foo = Foo { x :: Double, y :: String, z :: Foo } | Bar
  deriving (Show)

main = putStrLn (show (Foo 123 "abc" Bar))
