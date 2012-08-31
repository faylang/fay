{-# LANGUAGE NoImplicitPrelude #-}

main = print $ "Hello " ++ friends ++ family
  where friends = "my friends"
        family = " and family"

print :: String -> Fay ()
print = ffi "console.log(%1)"
