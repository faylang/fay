-- | Either operations.

module Data.Either where

import Prelude

-- | Basically forM.
whenLeft :: Either a b -> (a -> Fay c) -> Fay (Maybe c)
whenLeft (Left x) f = f x >>= return . Just
whenLeft (Right _) _ = return Nothing

-- | Basically forM.
whenRight :: Either a b -> (b -> Fay c) -> Fay (Maybe c)
whenRight (Right x) f = f x >>= return . Just
whenRight (Left _) _ = return Nothing

-- | Usual isLeft.
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Usual isRight.
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
