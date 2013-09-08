{-# LANGUAGE PackageImports #-}
module ExportQualified_Export (main, X.X) where

import           Prelude

import           "foo" X

main :: Fay ()
main = return ()
