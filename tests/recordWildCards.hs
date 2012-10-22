{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module RecordWildCards where

import Language.Fay.FFI
import Language.Fay.Prelude


------------------------------------------------------------------------------
data Person = Person
  { name :: String
  , surname :: String
  , age :: Int
  , wage :: Int
  }


------------------------------------------------------------------------------
main :: Fay ()
main = print (multAgeAndWage (Person "Alfredo" "Di Napoli" 10 10))


------------------------------------------------------------------------------
multAgeAndWage :: Person -> Int
multAgeAndWage (Person {name = "Alfredo", ..}) = age*wage
multAgeAndWage _ = 41


------------------------------------------------------------------------------
print :: Int -> Fay ()
print = ffi "console.log(%1)"
