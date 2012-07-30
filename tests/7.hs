main = print True

print :: Double -> Fay ()
print = ffi "console.log(%1)" FayNone
