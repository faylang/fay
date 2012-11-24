import           Language.Fay.Prelude

matchSame :: [a] -> ([a],[a])
matchSame x@y = (x,y)

matchSplit :: [a] -> ([a],a,[a])
matchSplit x@(y:z) = (x,y,z)

matchNested :: (a, [b]) -> ([b],b,[b])
matchNested (a,b@(x:xs)) = (b,x,xs)

main :: Fay ()
main = do
  print $ matchSame [1,2,3]
  print $ matchSplit [1,2,3]
  print $ matchNested (1, [1,2,3])
