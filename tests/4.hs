main = print "Hello, World!"

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""

