import Prelude

import ImportList1.A hiding (y)
import ImportList1.B hiding (x)

main :: Fay ()
main = do
  print x
  print y
