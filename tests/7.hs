main = print True

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" FayNone