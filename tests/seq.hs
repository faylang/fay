module Seq where

main = error "You shall not pass!" `seq` return ()
