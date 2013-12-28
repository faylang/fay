module CaseWildcard where

main = putStrLn (case False of
                  True -> "Hello!"
                  _    -> "Ney!")
