{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Language.Fay.FFI
import Language.Fay.Prelude hiding (show)

-- | Main entry point.
main :: Fay ()
main = do
  result <- documentGetElements "body"
  head <- documentGetElements "head"
  case result of
    (body:_) -> print body
    [] -> return ()

-- | Alert using window.alert.
print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""

data Element
instance Foreign Element

documentGetElements :: String -> Fay [Element]
documentGetElements = foreignFay "document.getElementsByTagName" "array"
