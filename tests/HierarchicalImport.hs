{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Hierarchical.Export

main :: Fay ()
main = printS exported

printS :: String -> Fay ()
printS = ffi "console.log(%1)"
