main = do print "Hello,"; print "World!"

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""
