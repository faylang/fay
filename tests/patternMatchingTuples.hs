-- compile with fay --html-wrapper
-- error thrown as soon as HTML page is loaded:
-- Uncaught TypeError: Cannot read property 'car' of null

import Prelude

main :: Fay ()
main = putStrLn doTest

doTest :: String
doTest = case ("x","") of
    (x : xs, c) -> "OK."
