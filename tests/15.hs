main = print (case False of
               True -> "Hello!"
               _    -> "Ney!")

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" FayNone