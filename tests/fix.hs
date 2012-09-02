{-# LANGUAGE NoImplicitPrelude #-}

module Fix where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main = print (head (tail (fix (\xs -> 123 : xs))))

print :: Double -> Fay ()
print = ffi "console.log(%1)"

head (x:xs) = x

fix f = let x = f x in x

tail (_:xs) = xs
