main = print (case False of
               True -> "Hello!"
               False -> "Ney!")

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""

