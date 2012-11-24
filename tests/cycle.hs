import Language.Fay.Prelude

main = mapM_ putStrLn (take 5 (cycle ["a", "b", "c"]))
