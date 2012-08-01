main = print "Hello, World!"

print :: String -> Fay ()
print = ffi "console.log(%1)"
