{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module RecordWildcards where

import           Language.Fay.FFI
import           Language.Fay.Prelude

data C = C { a :: Int, b :: Int, c :: Int, d :: Int }

data X = X { foo :: Int } | Y { foo :: Int }

f :: C -> Int
f (C {a, ..}) = a + d

test_fun :: C
test_fun = let {a=10; b=20; c=30; d=40} in C{..}

test2 :: X -> Int
test2 X{..} = foo

main = do
    let r = C{a=1, b=2, c=3, d=4}
    print_int (f r)
    print_c test_fun

    let x = X{foo=9}
    print_int (test2 x)

    -- TODO: is there a way to test for exceptions ?
    --let y = Y{foo=6}
    --print_int (test2 y)


print_int :: Int -> Fay ()
print_int = ffi "console.log(%1)"

print_c :: C -> Fay ()
print_c = ffi "console.log(%1)"
