{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import "base" Prelude
import Distribution.PackageDescription
import Distribution.Simple

main =
  defaultMainWithHooks simpleUserHooks
    { postInst = \_ _ _ _ -> putStrLn fayBaseReminder
    }

fayBaseReminder =
  "                                                                               \n\
  \- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n\
  \                                                                               \n\
  \                       You also need to install fay-base!                      \n\
  \                                                                               \n\
  \- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"
