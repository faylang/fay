main = do
  [1,2] <- return [1,2]
  print "OK."

print :: String -> Fay ()
print = ffi "console.log(%1)" FayNone
