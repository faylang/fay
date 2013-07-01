import Prelude

fmap :: (a -> b) -> Fay a -> Fay b
fmap f m = m >>= (return . f)

-- Note that this fixity declaration doesn't do anything except fix
-- typechecking since Fay ignores fixities other than the baseFixities
-- supplied to HSE. But since <$> is a base operator it's parsed as it
-- should be.
infixl 4 <$>
(<$>) :: (a -> b) -> Fay a -> Fay b
(<$>) = fmap

main :: Fay ()
main = print =<< ((+3) . (+2) <$> (return (1 :: Double)))
