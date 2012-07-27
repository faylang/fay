main = print ((\a 'a' -> "OK.") 0 'b')

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" FayNone