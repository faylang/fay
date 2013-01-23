{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import "base" Prelude
import Distribution.PackageDescription
import Distribution.Simple

main = do
  defaultMainWithHooks simpleUserHooks
    { preConf = \_args _flags -> do putStrLn reminder
                                    return emptyHookedBuildInfo
    , postInst = (\_ _ _ _ -> putStrLn fayBaseReminder)
    }

reminder =
  "                                                                               \n\
  \- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n\
  \                                                                               \n\
  \  REMEMBER: This compiler is in flux, supercalifragelistic style. You should   \n\
  \            read the CHANGELOG for this release as the changes probably        \n\
  \            affect you.                                                        \n\
  \                                                                               \n\
  \                                                                               \n\
  \                                                                               \n\
  \- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"

fayBaseReminder =
  "                                                                               \n\
  \- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n\
  \                                                                               \n\
  \                       You also need to install fay-base!                      \n\
  \                                                                               \n\
  \- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"
