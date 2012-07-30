main = print (case False of
               True -> "Hello!"
               False -> "Ney!")

print :: String -> Fay ()
print = ffi "console.log(%1)" FayNone
