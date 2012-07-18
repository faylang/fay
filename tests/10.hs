data Bool = True | False

main = print (concat ["Hello, ","World!"])

print = foreignJS 1 "console.log" ""

concat = foldr append []

foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

append (x:xs) ys = x : append xs ys
append []     ys = ys
