main = print (2 * 4 / 2)

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""
