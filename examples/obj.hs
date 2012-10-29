

module Print where

import Language.Fay.FFI
import Language.Fay.Prelude

data Foo = Foo { foo :: Double, bar :: String }
instance Foreign Foo

main :: Fay ()
main = do
  callback (Foo 123 "abc")
           (\(Foo n s) -> do printDouble n
                             printString s)

callback :: Foo -> (Foo -> Fay ()) -> Fay ()
callback = ffi "global.callback(%1,%2)"

printDouble :: Double -> Fay ()
printDouble = ffi "console.log(%1)"

printString :: String -> Fay ()
printString = ffi "console.log(%1)"
