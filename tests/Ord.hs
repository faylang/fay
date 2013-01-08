import Prelude

main = do
  when (1 < 2) $ putStrLn "Expected <"
  when (1 < 1) $ putStrLn "Unexpected < (1)"
  when (2 < 1) $ putStrLn "Unexpected < (2)"
  when (1 >= 2) $ putStrLn "Unexpected >="
  when (1 >= 1) $ putStrLn "Expected >= (1)"
  when (2 >= 1) $ putStrLn "Expected >= (2)"
  when (1 > 2) $ putStrLn "Unexpected > (1)"
  when (1 > 1) $ putStrLn "Unexpected > (2)"
  when (2 > 1) $ putStrLn "Expected >"
  when (1 <= 2) $ putStrLn "Expected <= (1)"
  when (1 <= 1) $ putStrLn "Expected <= (2)"
  when (2 <= 1) $ putStrLn "Unexpected <="
  print $ max 1 2
  print $ min 1 2
  case compare 1 2 of
    EQ -> putStrLn "FAIL (EQ)"
    LT -> putStrLn "WIN (LT)"
    GT -> putStrLn "FAIL (GT)"
