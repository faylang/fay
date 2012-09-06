{-# LANGUAGE NoImplicitPrelude #-}

main :: IO ()
main =
    let x = 10
    in print $ show (x + y)
  where y = 20

print :: String -> Fay ()
print = ffi "console.log(%1)"
