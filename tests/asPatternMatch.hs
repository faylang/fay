import           Prelude

matchSame :: [a] -> ([a],[a])
matchSame x@y = (x,y)

matchSplit :: [a] -> ([a],a,[a])
matchSplit x@(y:z) = (x,y,z)

matchNested :: (a, [b]) -> ([b],b,[b])
matchNested (a,b@(x:xs)) = (b,x,xs)

data XYZ = XYZ
  { foo :: String
  , bar :: Maybe String
  , baz :: Maybe String
  }

xyz = handleInfo $ XYZ "foo" (Just "bar") (Just "baz")
  where
    handleInfo :: XYZ -> String
    handleInfo ui@(XYZ _ Nothing _) = "error"
    handleInfo _ = "ok"

main :: Fay ()
main = do
  print $ matchSame [1,2,3]
  print $ matchSplit [1,2,3]
  print $ matchNested (1, [1,2,3])
  print $ xyz
