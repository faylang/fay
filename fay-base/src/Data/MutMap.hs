{-# LANGUAGE EmptyDataDecls #-}

module Data.MutMap
  ( -- * MutMap
    MutMap
  , mutEmpty
  , mutFromList
  , mutLookup
  , mutElems
  , mutKeys
  , mutAssocs
  , mutClone
  , mutMapM
  , mutMapM_
  , mutMapMaybeM
  , mutInsert
  , mutDelete
  , mutClear
  )
  where

import Data.Defined
import Data.MutMap.Internal
import Data.Text (Text)
import qualified Data.Text as T
import FFI
import Prelude

data MutMap a

-- Construction

mutEmpty :: Fay (MutMap a)
mutEmpty = ffi "{}"

mutFromList :: [(Text, a)] -> Fay (MutMap a)
mutFromList = mutFromListI . map (\(key, val) -> KeyValI (addSalt key) val)

mutFromListI :: [KeyValI a] -> Fay (MutMap a)
mutFromListI = ffi "function() { var r = {}; $.each(%1, function(ix, x) { r[x.slot1] = x.slot2; }); return r; }()"

-- Query

mutLookup :: Text -> MutMap a -> Fay (Maybe a)
mutLookup k m = return . fromDefined =<< mutLookupI (addSalt k) m

mutLookupI :: Salted -> MutMap a -> Fay (Defined a)
mutLookupI = ffi "%2[%1]"

mutElems :: MutMap a -> Fay [a]
mutElems = ffi "function() { var r = []; for (var k in %1) { r.push(%1[k]); } return r; }()"

mutKeys :: MutMap a -> Fay [Text]
mutKeys m = return . map unsalt =<< mutKeysI m

mutKeysI :: MutMap a -> Fay [Salted]
mutKeysI = ffi "function() { var r = []; for (var k in %1) { r.push(k); } return r; }()"

mutAssocs :: MutMap a -> Fay [(Text, a)]
mutAssocs m = return . map (\(KeyValI key val) -> (unsalt key, val)) =<< mutAssocsI m

mutAssocsI :: MutMap a -> Fay [KeyValI a]
mutAssocsI = ffi "function() { var r = []; for (var k in %1) { r.push({ instance : 'KeyValI', slot1 : k, slot2 : %1[k] }); } return r; }()"

mutClone :: MutMap a -> Fay (MutMap a)
mutClone = ffi "jQuery['extend']({}, %1)"

-- Note: Also clones.
mutMapM :: (a -> Fay b) -> MutMap a -> MutMap b
mutMapM = ffi "jQuery['map'](jQuery['extend']({}, %2), %1)"

mutMapM_ :: (a -> Fay ()) -> MutMap a -> Fay ()
mutMapM_ = ffi "jQuery['map'](%2, function(x) { %1(x); return x; })"

-- Note: Also clones.
mutMapMaybeM :: (a -> Fay (Maybe b)) -> MutMap a -> MutMap b
mutMapMaybeM f = mutMapMaybeMI $ \x -> f x >>= return . toDefined

mutMapMaybeMI :: (a -> Fay (Defined b)) -> MutMap a -> MutMap b
mutMapMaybeMI = ffi "jQuery['map']($['extend']({}, %2), %1)"

-- Mutation

mutInsert :: Text -> a -> MutMap a -> Fay ()
mutInsert = mutInsertI . addSalt

mutInsertI :: Salted -> a -> MutMap a -> Fay ()
mutInsertI = ffi "%3[%1] = %2"

mutDelete :: Text -> MutMap a -> Fay ()
mutDelete = mutDeleteI . addSalt

mutDeleteI :: Salted -> MutMap a -> Fay ()
mutDeleteI = ffi "delete %2[%1]"

mutClear :: MutMap a -> Fay ()
mutClear = ffi "function() { for (var k in %1) { delete %1[k]; } }()"

{- NOTE: I put out the effort to write these, but they are untested and ended
   up being unnecessary..

-- Reinserts everything into an object, in order to force serialization, using
-- the still-salted keys.  'Nothing' indicates a removal.
mutToObject :: (a -> Fay (Maybe (Automatic b))) -> MutMap a -> Fay Object
mutToObject f m = do
  obj <- objNew
  mutAssocsI m >>= mapM (\(KeyValI k v) -> do
      mv <- f v
      whenJust mv $ \v' -> objInsert (unsafeCoerce k) v' obj
    )
  return obj

objectToMut :: (Automatic b -> Fay (Maybe a)) -> Object -> Fay (MutMap a)
objectToMut f obj = do
  let obj' = unsafeCoerce obj
  m <- mutEmpty
  mutAssocsI obj' >>= mapM (\(KeyValI k v) -> do
      mv <- f v
      whenJust mv $ \v' -> when (checkSalted k) $ mutInsertI k v' m
    )
  return m
-}
