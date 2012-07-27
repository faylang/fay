-- Generate Show value.
-- 
-- $ fay -autorun examples/data.hs
-- $ node examples/data.js
-- (Foo { x = 123, y = "abc", z = (Bar) })

{-# LANGUAGE NoImplicitPrelude #-}

module Data where

import Language.Fay.Prelude
import Language.Fay.FFI

data Foo = Foo { x :: Double, y :: String, z :: Foo } | Bar
  deriving (Show)
instance Foreign Foo

main = print (show (Foo 123 "abc" Bar))

print :: String -> Fay ()
print = foreignFay "console.log" FayNone