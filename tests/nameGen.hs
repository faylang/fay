import           Language.Fay.FFI
import           Language.Fay.Prelude

data SomeRec = SomeRec { a :: Integer, b :: Integer } | Y | X


main = do
    -- https://github.com/faylang/fay/issues/121
    print_str $ case Y of
                    SomeRec _ _ -> "Bad"
                    Y -> "OK."

    let t = Y
    print_str $ case t of
                    SomeRec _ _ -> "Bad"
                    Y -> "OK."


print_str :: String -> Fay ()
print_str = ffi "console.log(%1)"
