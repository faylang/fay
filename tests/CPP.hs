{-# LANGUAGE CPP #-}
#ifdef FAY
import           Prelude
#else
this is invalid code
#endif

#ifdef FAY
main :: Fay ()
#endif

#ifndef FAY
more invalid code
#if FAY
this should not be used
#endif
#else
#if FAY
main = print True
#else
invalid and nested
#endif
#endif
