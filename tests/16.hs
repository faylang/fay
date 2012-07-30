data Person = Person String String Int

main = print (case Person "Chris" "Done" 13 of
                Person "Chris" "Done" 13 -> "Hello!")

print :: String -> Fay ()
print = ffi "console.log(%1)" FayNone
