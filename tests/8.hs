data Bool = True | False

main = print (head (fix (\xs -> 123 : xs)))

print :: Double -> Fay ()
print = ffi "console.log(%1)" FayNone

head (x:xs) = x

fix f = let x = f x in x
