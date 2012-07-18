data Bool = True | False

main = print (take 5 (let ns = 1 : map (\x -> x + 1) ns in ns))

print = foreignJS 1 "console.log" ""

take 0 _      = []
take n (x:xs) = x : take (n - 1) xs

map f []     = []
map f (x:xs) = f x : map f xs