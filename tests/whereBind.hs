import           Language.Fay.Prelude

main :: Fay ()
main =
    let x = 10
    in putStrLn $ show (x + y)
  where y = 20

