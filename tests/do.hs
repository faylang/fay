{-# LANGUAGE NoImplicitPrelude #-}

main = do print "Hello,"; print "World!"

print :: String -> Fay ()
print = ffi "console.log(%1)"
