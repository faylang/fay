main = print (5 * 3 / 2)

print :: Double -> Fay ()
print = ffi "console.log(%1)" FayNone