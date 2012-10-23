{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns #-}

module NamedFieldPuns where

import           Language.Fay.FFI
import           Language.Fay.Prelude

data SomeRec = SomeRec { a :: Integer, b :: Integer }

fun :: SomeRec -> SomeRec
fun SomeRec{a} = SomeRec{a=a+1, b=10}

fun2 :: SomeRec -> SomeRec
fun2 r = let a = 5 in r{a}

main = do
    let r = SomeRec{a=1, b=2}
    print_rec r
    print_rec (fun r)
    print_rec (fun2 r)

print_rec :: SomeRec -> Fay ()
print_rec = ffi "console.log(%1)"
