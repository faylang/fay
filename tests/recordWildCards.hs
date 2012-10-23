{-# LANGUAGE NoImplicitPrelude,
             RecordWildCards,
             NamedFieldPuns #-}

module RecordWildcards where

--import Prelude
import           Language.Fay.FFI
import           Language.Fay.Prelude

data C = C { a :: Int, b :: Int, c :: Int, d :: Int }
    deriving Show

f :: C -> Int
f (C {a, ..}) = a + d

test_fun :: C
test_fun = let {a=10; b=20; c=30; d=40} in C{..}

main = do
    let r = C{a=1, b=2, c=3, d=4}
    print_integer (f r)
    print_c test_fun

print_integer :: Int -> Fay ()
print_integer = ffi "console.log(%1)"

print_c :: C -> Fay ()
print_c = ffi "console.log(%1)"
