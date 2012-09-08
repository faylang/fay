{-# LANGUAGE NoImplicitPrelude #-}

import Language.Fay.FFI
import Language.Fay.Prelude

data R = R { i :: Double, c :: Char }
data S = S Double Char

-- RecDecl
r1 :: R
r1 = R { i = 1, c = 'a' }

-- RecDecl with fields out of order
r2 :: R
r2 = R { c = 'b', i = 2 }

-- Partial RecDecl (Produces GHC warning)
r' :: R
r' = R { c = 'b' }

-- Regular application
r3 :: R
r3 = R 3 'c'

-- Using regular constructor
s1 :: S
s1 = S 1 'a'

-- Multiple fields with the same type
data X = X { _x1, _x2 :: Int }
x1 = X 1 2
x2 = X { _x1 = 1, _x2 = 2 }

-- Record updates
r1' = r1{ i = 10 }
r2' = r2{ c = 'a', i = 20 }
r'' = r'{ i = 123 }

main = do
  -- print updated records first to show that old records are preserved
  print r1'
  print r2'
  print r''

  print r1
  printS (show (i r1))
  printS (show (c r1))
  print r2
  print r'
  print r3
  print s1
  print x1
  print x2

printS :: String -> Fay ()
printS = ffi "console.log(%1)"

print :: a -> Fay ()
print = ffi "console.log(%1)"
