data Bool = True | False

data Person = Person String String Int

main = print (case Person "Chris" "Done" 13 of
                Person "Chris" "Done" 13 -> "Hello!")

print = foreignJS 1 "console.log" ""
