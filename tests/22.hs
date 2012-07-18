data Bool = True | False

main = do
  x <- return "Hello, World!" >>= return
  print x

print = foreignJS 1 "console.log" ""
