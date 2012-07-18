data Bool = True | False

main = print (case False of
               True -> "Hello!"
               False -> "Ney!")

print = foreignJS 1 "console.log" ""
