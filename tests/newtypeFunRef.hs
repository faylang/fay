import  Prelude

newtype Parser a = Parser { runParser :: String -> Either String (a, String) }

p = Parser
pure a = p (\s -> Right (a,s))

main :: Fay ()
main = print $ runParser (pure 5) "(0, 0, 0)"
