data Math = Math
  { root :: Double -> Double
  , square :: Double -> Double
  , cube :: Double -> Double
  }

math = Math sqrt
            (\x -> x * x)
            (\x -> x * square math x)

rootroot = root math (root math 5)