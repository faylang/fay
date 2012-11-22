import           Language.Fay.FFI
import           Language.Fay.Prelude

import           ExportType

w' :: W
w' = w

main :: Fay ()
main = do
  log X
  log Y
  log (Z 1)
  log (z (Z 1))
  log w'
  log (V 1 2)
  log (v1 (V 1 2))

log :: Foreign f => f -> Fay ()
log = ffi "console.log(%1)"
