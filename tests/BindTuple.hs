{-# LANGUAGE NoImplicitPrelude #-}

module BindTuple where

import           Language.Fay.FFI
import           Language.Fay.Prelude

unPackCase tup = case tup of (a, b) -> a

unPackLet tup = let (a, b) = tup in b

undefined :: Foreign a => Fay a
undefined = ffi "undefined"

unPackNested =
  let ((a, b), c) = (("Hi", "From"), "Nested")
  in concat $ intersperse " " [a, b, c]

unPackFnApplication str =
    let (a, b) = f str
    in a ++ " " ++  b
    where f str = ("Hi", str)

main = do
  let tup = ("Hi", "There")
  print $ unPackCase tup
  print $ unPackLet tup
  print $ unPackFnApplication "Again"
  print unPackNested


print :: String -> Fay ()
print = ffi "console.log(%1)"
