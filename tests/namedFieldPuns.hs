{-# LANGUAGE NamedFieldPuns #-}

import           Language.Fay.FFI
import           Language.Fay.Prelude

data SomeRec = SomeRec { a :: Integer, b :: Integer } | Y | X
instance Foreign SomeRec

fun :: SomeRec -> SomeRec
fun SomeRec{a} = SomeRec{a=a+1, b=10}

fun2 :: SomeRec -> SomeRec
fun2 r = let a = 5 in r{a}

main = do
    let r = SomeRec{a=1, b=2}
    print_rec r
    print_rec (fun r)
    print_rec (fun2 r)

    -- https://github.com/faylang/fay/issues/121
    let t = Y
    print_str $ case t of
                    SomeRec{a} -> "Bad"
                    Y -> "OK."

print_rec :: SomeRec -> Fay ()
print_rec = ffi "console.log(%1)"

print_str :: String -> Fay ()
print_str = ffi "console.log(%1)"
