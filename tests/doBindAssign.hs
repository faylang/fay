module DoBindAssign where

main :: Fay ()
main = do
  x <- return "Hello, World!" >>= return
  putStrLn x
