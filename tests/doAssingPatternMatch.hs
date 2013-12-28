module DoAssingPatternMatch where

main :: Fay ()
main = do
  [1,2] <- return [1,2]
  putStrLn "OK."
