import Prelude

poseL :: Bool -> String
poseL y | y == True = "Not OK"
  where x = 5
poseL _ = "OK"

main :: Fay ()
main = putStrLn $ poseL False
