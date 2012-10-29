-- | Simple example interfacing with node.

{-# LANGUAGE EmptyDataDecls    #-}


module Test where

import           Language.Fay.FFI
import           Language.Fay.Prelude

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
require' = ffi "require(%1)"

inspect :: Foreign a => Sys -> a -> Fay Details
inspect = ffi "%1.inspect(%2)"

print :: (Foreign a,Show a) => a -> Fay ()
print = ffi "console.log(%1)"
