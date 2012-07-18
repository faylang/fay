main = print 1

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""
