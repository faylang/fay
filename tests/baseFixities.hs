import Prelude

fmap :: (a -> b) -> Fay a -> Fay b
fmap f m = m >>= (return . f)

(<$>) :: (a -> b) -> Fay a -> Fay b
(<$>) = fmap

main :: Fay ()
main = print =<< ((+3) . (+2) <$> (return 1))
