main = print (take 5 (let ns = 1 : map (\x -> x + 1) ns in ns))

take 0 _      = []
take n (x:xs) = x : take (n - 1) xs

map f []     = []
map f (x:xs) = f x : map f xs

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" ""
