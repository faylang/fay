{-# LANGUAGE NoImplicitPrelude #-}

import           Hierarchical.RecordDefined

f :: R -> Integer
f (R i) = i

main = do
  print $ f (R 1)
  print $ g (Callback 1)

data R = R Integer

print :: Double -> Fay ()
print = ffi "console.log(%1)"
