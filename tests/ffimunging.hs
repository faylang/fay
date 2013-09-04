{-# LANGUAGE NoImplicitPrelude #-}

module Maybe where

import           FFI
import           Prelude

data Munge a b = Fudge a b

data Foo = Foo (Munge [String] [Int])

munge :: Foo -> Foo
munge = ffi "%1"

sponge :: Munge [String] [Int] -> Munge [String] [Int]
sponge = ffi "%1"

main = do
  case munge (Foo (Fudge ["a","b"] [1,2])) of
    Foo (Fudge xs is) -> do printS xs
                            printI is
  case sponge (Fudge ["a","b"] [1,2]) of
    Fudge xs is -> do printS xs
                      printI is

printS :: [String] -> Fay ()
printS = ffi "console.log(%1)"

printI :: [Int] -> Fay ()
printI = ffi "console.log(%1)"
