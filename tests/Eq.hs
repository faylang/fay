import Prelude

main = do
  when (1 == 1) $ putStrLn "Expected =="
  when (1 == 2) $ putStrLn "Unexpected =="
  when (1 /= 1) $ putStrLn "Unexpected /="
  when (1 /= 2) $ putStrLn "Expected /="
