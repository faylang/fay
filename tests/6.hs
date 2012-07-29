main = print (10 + (2 * (4 / 2)))

print :: Double -> Fay ()
print = ffi "console.log(%1)" FayNone