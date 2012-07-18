data Bool = True | False

main = print (case True of
               True -> "Hello!"
               False -> "Ney!")

print = foreignJS 1 "console.log" ""
