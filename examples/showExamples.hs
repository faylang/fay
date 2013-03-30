import           FFI
import           Prelude

main = do
  let a1 = ("string",1) :: (String,Int)
  putStrLn $ showPair a1
  putStrLn $ showList [1..3]
  putStrLn $ showString "Just a plain string"
  l1 <- readList "[4,5,6]"
  putStrLn . showList . map (1+) $ l1
  (s,i) <- readPair "[\"first\",2]"
  putStrLn $ s ++ " was first and " ++ showInt i ++ " was second"

showInt :: Int -> String
showInt = ffi "JSON.stringify(%1)"

showString :: String -> String
showString = ffi "JSON.stringify(%1)"

showPair :: (String,Int) -> String
showPair = ffi "JSON.stringify(%1)"

showList :: [Int] -> String
showList = ffi "JSON.stringify(%1)"

readList :: String -> Fay [Int]
readList = ffi "JSON.parse(%1)"

readPair :: String -> Fay (String,Int)
readPair = ffi "JSON.parse(%1)"

{-

compile with:
 fay showExamples.hs
run with: (on Debian)
 nodejs showExamples.hs

Produces:

["string",1]
[1,2,3]
"Just a plain string"
[5,6,7]
first was first and 2 was second


-}
