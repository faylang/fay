import FFI
import Prelude

f :: (Int,Double) -> Double
f = ffi "%1[0]+%1[1]"

g :: Double -> (Int,Double)
g = ffi "[Math.floor(%1),Math.round(100*(%1-Math.floor(%1)))/100]"

h :: (String,Int) -> (Int,String)
h = ffi "[%1[0]['length'],'#'+%1[1]['toString']()+'#']"

main = do
  print $ f (1,2.3)
  print $ fst (g 8.76)
  print $ snd (g 8.76)
  print $ fst (h ("abc",12))
  putStrLn $ snd (h ("abc",12))

