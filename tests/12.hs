main = print (show (take 5 (let ns = 1 : map (foo 123) ns in ns)))

foo x y = x * y / 2

take 0 _      = []
take n (x:xs) = x : take (n - 1) xs

map f []     = []
map f (x:xs) = f x : map f xs

print :: String -> Fay ()
print = ffi "console.log(%1)" FayNone

show :: String -> String
show = ffi "JSON.stringify(%1)" FayString
