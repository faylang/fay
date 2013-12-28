module RecCon where

data Bool = True | False

main :: Fay ()
main = print (head (fix (\xs -> 123 : xs)))

fix f = let x = f x in x
