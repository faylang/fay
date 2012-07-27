main = do
  [1,2] <- return [1,2]
  print "OK."

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" FayNone