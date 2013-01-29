module Console (main) where

main = putStrLn "Hello, World!"

putStrLn :: String -> Fay ()
putStrLn = ffi "console.log(%1)"
