data Bool = True | False

main = do print "Hello,"; print "World!"

print = foreignJS 1 "console.log" ""
