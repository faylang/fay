-- | Maybe functions.

module Data.Maybe
  (-- * General operations from base
  isJust
  ,isNothing
  ,fromJust
  ,fromMaybe
  ,maybeToList
  ,listToMaybe
  ,catMaybes
  ,mapMaybe
  ,mapMaybeFB
  -- * Fay helpers
  ,whenJust
  ,whenJust'
  ,onJust
  ,joinMaybe)
 where

import Prelude

-- ---------------------------------------------------------------------------
-- Functions over Maybe

-- | The 'isJust' function returns 'True' iff its argument is of the
-- form @Just _@.
isJust         :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

-- | The 'isNothing' function returns 'True' iff its argument is 'Nothing'.
isNothing         :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- | The 'fromJust' function extracts the element out of a 'Just' and
-- throws an error if its argument is 'Nothing'.
fromJust          :: Maybe a -> a
fromJust Nothing  = error "Maybe.fromJust: Nothing" -- yuck
fromJust (Just x) = x

-- | The 'fromMaybe' function takes a default value and and 'Maybe'
-- value.  If the 'Maybe' is 'Nothing', it returns the default values;
-- otherwise, it returns the value contained in the 'Maybe'.
fromMaybe     :: a -> Maybe a -> a
fromMaybe d x = case x of {Nothing -> d;Just v  -> v}

-- | The 'maybeToList' function returns an empty list when given
-- 'Nothing' or a singleton list when not given 'Nothing'.
maybeToList            :: Maybe a -> [a]
maybeToList  Nothing   = []
maybeToList  (Just x)  = [x]

-- | The 'listToMaybe' function returns 'Nothing' on an empty list
-- or @'Just' a@ where @a@ is the first element of the list.
listToMaybe           :: [a] -> Maybe a
listToMaybe []        =  Nothing
listToMaybe (a:_)     =  Just a

-- | The 'catMaybes' function takes a list of 'Maybe's and returns
-- a list of all the 'Just' values.
catMaybes              :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

-- | The 'mapMaybe' function is a version of 'map' which can throw
-- out elements.  In particular, the functional argument returns
-- something of type @'Maybe' b@.  If this is 'Nothing', no element
-- is added on to the result list.  If it just @'Just' b@, then @b@ is
-- included in the result list.
mapMaybe          :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) =
 let rs = mapMaybe f xs in
 case f x of
  Nothing -> rs
  Just r  -> r:rs

mapMaybeFB :: (b -> r -> r) -> (a -> Maybe b) -> a -> r -> r
mapMaybeFB cons f x next = case f x of
  Nothing -> next
  Just r -> cons r next

-- | Handy alternative to not having forM.
whenJust :: Maybe a -> (a -> Fay ()) -> Fay ()
whenJust (Just x) f = f x
whenJust Nothing _ = return ()

-- | Similar to forM again.
whenJust' :: Maybe a -> (a -> Fay b) -> Fay (Maybe b)
whenJust' (Just x) f = f x >>= return . Just
whenJust' Nothing _ = return Nothing

-- | Basically fmap for Maybe.
onJust :: (a -> b) -> Maybe a -> Maybe b
onJust f (Just x) = Just (f x)
onJust _ Nothing = Nothing

-- | Join for Maybe.
joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe (Just (Just x)) = Just x
joinMaybe _ = Nothing
