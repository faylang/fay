module TupleRefs where

import Language.Fay.FFI
import Language.Fay.Prelude

main :: Fay ()
main = do
  tstref <- newRef ((0,0) :: (Double,Double))
  writeRef tstref (5.3,4.2)
  tstval <- readRef tstref
  print $ fst tstval
  print $ snd tstval

data Ref a
instance Foreign a => Foreign (Ref a)

newRef :: Foreign a => a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref(%1)"

writeRef :: Foreign a => Ref a -> a -> Fay ()
writeRef = ffi "Fay$$writeRef(%1,%2)"

readRef :: Foreign a => Ref a -> Fay a
readRef = ffi "Fay$$readRef(%1)"

print :: Foreign a => a -> Fay ()
print = ffi "console.log(%1)"
