import           Prelude

data Bool = True | False

main = print (head (fix (\xs -> 123 : xs)))

fix f = let x = f x in x
