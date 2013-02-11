import           Prelude
import           NewtypeImport_Export

x = case MyInteger undefined of
      MyInteger _ -> 1

y = case undefined of
      MyInteger _ -> 1

int :: Int
int = undefined

yInt = case int of
         _ -> 1

main = do
    print x
    print y
    print yInt
    print (Baz (Bar 1))
    baz <- getBaz
    print baz
    case baz of
      Baz (Bar i) -> print i
    print (bar $ unwrapBaz baz)
