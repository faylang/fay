main = print (case True of
               True -> "Hello!"
               False -> "Ney!")

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" FayNone