data R = R { i :: Integer, c :: Char }
data S = S Integer Char

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

main = do
  print r1
  printS (show (i r1))
  printS (show (c r1))
  print r2
  print r'
  print r3
  print s1

printS :: String -> Fay ()
printS = ffi "console.log(%1)"

print :: a -> Fay ()
print = ffi "console.log(%1)"
