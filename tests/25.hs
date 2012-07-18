data Bool = True | False

main = print ((\a 'a' -> "OK.") 0 'b')

print = foreignJS 1 "console.log" ""
