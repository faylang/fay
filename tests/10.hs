main = print (concat ["Hello, ","World!"])

concat = foldr append []

foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

append (x:xs) ys = x : append xs ys
append []     ys = ys

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""
