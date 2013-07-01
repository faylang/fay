import           Prelude

main = print (head (tail (fix (\xs -> 123 : xs))))

fix f = let x = f x in x
