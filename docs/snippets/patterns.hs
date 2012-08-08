data Math = Math
  { root :: Double -> Double
  , square :: Double -> Double
  , cube :: Double -> Double
  }

race (winner,runners) = print (winner,runners)

first (x:xs) = x

getSquare (Math _ square _) = square
