data Bool = True | False

main = print (head (fix (\xs -> 123 : xs)))

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""

head (x:xs) = x

fix f = let x = f x in x
