data Bool = True | False

main = print (head (fix (\xs -> 123 : xs)))

print = foreignJS 1 "console.log" ""

head (x:xs) = x

fix f = let x = f x in x
