import           Prelude

import           Hierarchical.RecordDefined

f :: R -> Double
f (R i) = i

main = do
  print $ f (R 1)
  print $ g (Callback 1)

data R = R Double

