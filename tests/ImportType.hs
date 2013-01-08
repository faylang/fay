import           Prelude

import           ExportType

w' :: W
w' = w

main :: Fay ()
main = do
  print X
  print Y
  print (Z 1)
  print (z (Z 1))
  print w'
  print (V 1 2)
  print (v1 (V 1 2))

