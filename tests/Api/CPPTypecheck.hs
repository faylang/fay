{-# LANGUAGE CPP #-}

module CPPTypecheck where

#ifdef FAY
import           FFI
import           Prelude
#endif

data R = R { time :: String }
instance Foreign R

main :: Fay ()
main = print $ R "x"
