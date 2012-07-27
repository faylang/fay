main = print (head (tail (fix (\xs -> 123 : xs))))

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" FayNone

head (x:xs) = x

fix f = let x = f x in x

tail (_:xs) = xs
