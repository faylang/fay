import           Prelude

main = do
  x <- return "Hello, World!" >>= return
  putStrLn x

