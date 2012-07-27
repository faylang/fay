main = print (5 * 3 / 2)

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" FayNone