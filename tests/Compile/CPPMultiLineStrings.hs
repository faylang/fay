{-# LANGUAGE NoImplicitPrelude #-}
module CPPMultiLineStrings where

import Prelude
import FFI

printAll :: [String] -> Fay ()
printAll = ffi "\
  \(function(xs) {\
  \  var prn = function(x) {\
  \    console.log(x);\
  \  };\
  \  for(var i=0; i<xs.length; i++) {\
  \    prn(xs[i]);\
  \  }\
  \})(%1)"

main :: Fay ()
main = printAll ["a", "b", "c"]
