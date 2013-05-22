import FFI
import Prelude

main = do
  f <- getF
  g <- getG
  h <- getH 0
  ret1 <- f "hey" "world" 123
  ret2 <- f "ahoy" "la" 666
  ret3 <- g "ack"
  putStrLn ret1
  putStrLn ret2
  putStrLn ret3
  putStrLn (h "apples" "pears")

getF :: Fay (String -> String -> Int -> Fay String)
getF = ffi "(function(x,y,z){ return 'getF: ' + x + ', ' + y + ', ' + z; })"

getG :: Fay (String -> Fay String)
getG = ffi "(function(x){ return 'getG: ' + x; })"

getH :: Int -> Fay (String -> String -> String)
getH = ffi "(function(x,y){ return 'getH: ' + x + ', ' + y; })"
