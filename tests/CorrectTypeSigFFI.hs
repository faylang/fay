module CorrectTypeSigFFI where

import FFI

int = 0
int :: Int

toplevel = ffi "\"one \""
toplevel :: String

multipleToplevel, insideWhere :: String
multipleToplevel = ffi "\"two \""

insideWhere = "three "
  -- where
  --   str :: String
  --   str = ffi "\"three \""

expression = "f" ++ (ffi "\"our\"" :: String)

main = putStrLn $ concat [toplevel, multipleToplevel, insideWhere, expression]
