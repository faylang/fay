{-# LANGUAGE CPP #-}
#ifdef FAY
import           Language.Fay.Prelude
#else
this is invalid code
#endif

#ifdef FAY
main :: Fay ()
#endif

#ifndef FAY
more invalid code
#else
#if FAY
main = print True
#else
invalid and nested
#endif
#endif