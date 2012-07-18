data Bool = True | False

main = do
  [1,2] <- return [1,2]
  print "OK."

print = foreignJS 1 "console.log" ""
