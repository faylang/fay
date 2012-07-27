-- | Simple example interfacing with node.

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test where

import Language.Fay.FFI
import Language.Fay.Prelude

data Sys
instance Foreign Sys

data Details
instance Show Details
instance Foreign Details

main = do
  sys <- require' "sys"
  npm <- require' "npm"
  details <- inspect sys npm
  print $ details

require' :: String -> Fay Sys
require' = foreignFay "require" FayNone

inspect :: Foreign a => Sys -> a -> Fay Details
inspect = foreignMethodFay "inspect" FayNone

print :: (Foreign a,Show a) => a -> Fay ()
print = foreignFay "console.log" FayNone