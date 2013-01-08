import Prelude

main :: Fay ()
main = let y = x + 1
           x = y + 1
       in print y

