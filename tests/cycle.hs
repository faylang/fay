import Language.Fay.Prelude
import Language.Fay.FFI

putStrLn :: String -> Fay ()
putStrLn = ffi "console.log(%1)"

main = mapM_ putStrLn (take 5 (cycle ["a", "b", "c"]))
