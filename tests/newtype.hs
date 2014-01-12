module Newtype where

import FFI

newtype MyInteger = MyInteger Int

x = case MyInteger undefined of
      MyInteger _ -> 1

y = case undefined of
      MyInteger _ -> 1

int :: Int
int = undefined

yInt = case int of
         _ -> 1

data Foo = Bar { bar :: Double }
newtype Baz = Baz { unwrapBaz :: Foo }

getBaz :: Fay Baz
getBaz = ffi "{ instance: 'Bar', bar: 1 }"

getBazExpr = ffi "{ instance: 'Bar', bar : 2 }" :: Fay Baz

getBazExpr' = f (Baz (Bar 2)) (ffi "{ instance: 'Bar', bar : 3 }" :: Baz)
f :: Baz -> Baz -> Baz
f (Baz x) (Baz y) = Baz (Bar (bar x + bar y))


main = do
    print x
    print y
    print yInt
    print (Baz (Bar 1))

    baz <- getBaz
    print baz
    case baz of Baz (Bar i) -> print i
    print (bar $ unwrapBaz baz)

    bazExpr <- getBazExpr
    print bazExpr
    case bazExpr of Baz (Bar i) -> print i
    print (bar $ unwrapBaz bazExpr)

    let bazExpr' = getBazExpr'
    print bazExpr'
    case bazExpr' of Baz (Bar i) -> print i
    print (bar $ unwrapBaz bazExpr')
