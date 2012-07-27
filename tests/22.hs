main = do
  x <- return "Hello, World!" >>= return
  print x

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" FayNone