{-# LANGUAGE NoImplicitPrelude #-}

module Console where

import Language.Fay.FFI
import Language.Fay.Prelude

main = do
  print "What's"
  msg <- sync $ timeoutPass "It's peanut butter jelly time!"
  print $ "the message? " ++ msg

  print "Hello"
  sync $ setTimeout 500
  print "friends!"

setTimeout :: Double -> (() -> Fay ()) -> Fay ()
setTimeout = ffi "setTimeout(%2,%1)"

-- | Time out and finally pass a value back to the function.
timeoutPass :: String -> (String -> Fay ()) -> Fay ()
timeoutPass = ffi "setTimeout(function(){ %2(%1); },500)"

-- | Print using console.log.
print :: String -> Fay ()
print = ffi "console.log(%1)"
