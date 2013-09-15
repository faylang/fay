module QualifiedImport where

import           FFI
import           Prelude

import qualified QualifiedImport.X
import qualified QualifiedImport.X as X
import qualified QualifiedImport.Y as Y

main :: Fay ()
main = do
  print QualifiedImport.X.x
  print Y.y
  print X.X3 { X.x4 = 1 }
  print $ X.X3 2
  print (X.X3 3) { X.x4 = 4 }
  case X.X3 4 of
    X.X3 { X.x4 = n } -> print n
