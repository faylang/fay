{-# LANGUAGE NoImplicitPrelude #-}

module RecordFunctionPatternMatch where

import           Language.Fay.FFI
import           Language.Fay.Prelude

data Person = Person String String Int

main = print (foo (Person "Chris" "Done" 14))

foo (Person "Chris" "Done" 13) = "Foo!"
foo (Person "Chris" "Barf" 14) = "Bar!"
foo (Person "Chris" "Done" 14) = "Hello!"
foo _ = "World!"

print :: String -> Fay ()
print = ffi "console.log(%1)"
