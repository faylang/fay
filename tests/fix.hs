import           Language.Fay.Prelude

main = print (head (tail (fix (\xs -> 123 : xs))))

head (x:xs) = x

fix f = let x = f x in x

tail (_:xs) = xs
