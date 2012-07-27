module Main where

import Distribution.PackageDescription
import Distribution.Simple

main = defaultMainWithHooks simpleUserHooks
       { preConf = \_args _flags -> do putStrLn reminder
                                       return emptyHookedBuildInfo
       }

reminder =
  "                                                                               \n\
  \- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n\
  \                                                                               \n\
  \  REMEMBER: This compiler is in flux, supercalifragelistic style. You should   \n\
  \            read the CHANGELOG for this release as the changes probably        \n\
  \            affect you.                                                        \n\
  \                                                                               \n\
  \- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n"
