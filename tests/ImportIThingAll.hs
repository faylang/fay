import           Prelude
import           FFI

import           ImportList1.C        (A (..))

main :: Fay ()
main = do
  print $ B1 1
  print $ b1 (B1 1)
  print $ B2 2
