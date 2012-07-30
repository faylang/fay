main = do
  x <- return "Hello, World!" >>= return
  print x

print :: String -> Fay ()
print = ffi "console.log(%1)" FayNone
