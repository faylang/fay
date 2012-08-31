{-# LANGUAGE NoImplicitPrelude #-}

main = print ((\a 'a' -> "OK.") 0 'b')

print :: String -> Fay ()
print = ffi "console.log(%1)"
