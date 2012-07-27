data Person = Person String String Int

main = print (case Person "Chris" "Done" 13 of
                Person "Chris" "Done" 13 -> "Hello!")

print :: Foreign a => a -> Fay ()
print = foreignFay "console.log" FayNone