main = print 1

print :: Double -> Fay ()
print = ffi "console.log(%1)" FayNone
