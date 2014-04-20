{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS -fno-warn-type-defaults #-}

-- | Convert a Haskell value to a (JSON representation of a) Fay value.

module Fay.Convert
  (showToFay
  ,readFromFay
  ,readFromFay'
  ,encodeFay
  ,decodeFay)
  where

import           Control.Arrow (first)
import           Control.Monad.State
import           Control.Spoon
import           Data.Aeson
import           Data.Aeson.Types (parseEither)
import           Data.Data
import           Data.Generics.Aliases
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as Map
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Vector            (Vector)
import qualified Data.Vector            as Vector

--------------------------------------------------------------------------------
-- The conversion functions.

-- | Convert a Haskell value to a Fay json value.  This can fail when primitive
--   values aren't handled by explicit cases.  'encodeFay' can be used to
--   resolve this issue.
showToFay :: Data a => a -> Maybe Value
showToFay = encodeFay (\x -> x)

-- | Convert a Haskell value to a Fay json value.  This can fail when primitive
--   values aren't handled by explicit cases.  When this happens, you can add
--   additional cases via the first parameter.
--
--   The first parameter is a function that can be used to override the
--   conversion.  This usually looks like using 'extQ' to additional type-
--   specific cases.
encodeFay :: (GenericQ Value -> GenericQ Value) -> GenericQ (Maybe Value)
encodeFay specialCases = spoon . encodeFayInternal specialCases

encodeFayInternal :: (GenericQ Value -> GenericQ Value) -> GenericQ Value
encodeFayInternal specialCases = specialCases $
    encodeGeneric rec
    `extQ` unit
    `extQ` Bool
    `extQ` (toJSON :: Int -> Value)
    `extQ` (toJSON :: Float -> Value)
    `extQ` (toJSON :: Double -> Value)
    `extQ` (toJSON :: Rational -> Value)
    `ext1Q` list
    `extQ` string
    `extQ` char
    `extQ` text
  where
    rec :: GenericQ Value
    rec = encodeFayInternal specialCases
    unit () = Null
    list :: Data a => [a] -> Value
    list = Array . Vector.fromList . map rec
    string = String . Text.pack
    char = String . Text.pack . (:[])
    text = String

encodeGeneric :: GenericQ Value -> GenericQ Value
encodeGeneric rec x =
    case constrName of
      '(':(dropWhile (==',') -> ")") ->
        Array $ Vector.fromList $ gmapQ rec x
      _ -> Object $ Map.fromList $ map (first Text.pack) fields
  where
    fields =
      ("instance", String $ Text.pack constrName) :
      zip labels (gmapQ rec x)
    constrName = showConstr constr
    constr = toConstr x
    -- Note: constrFields can throw errors for non-algebraic datatypes.  These
    -- ought to be taken care of in the other cases of encodeFay.
    labels = case constrFields constr of
      [] -> map (("slot"++).show) [1::Int ..]
      ls -> ls

-- | Convert a Fay json value to a Haskell value.
readFromFay :: Data a => Value -> Maybe a
readFromFay = either (\_ -> Nothing) Just . decodeFay (\_ -> id)

-- | Convert a Fay json value to a Haskell value.  This is like readFromFay,
--   except it yields helpful error messages on failure.
readFromFay' :: Data a => Value -> Either String a
readFromFay' = decodeFay (\_ -> id)

-- | Convert a Fay json value to a Haskell value.
--
--   The first parameter is a function that can be used to override the
--   conversion.  This usually looks like using 'extR' to additional type-
--   specific cases.
decodeFay :: Data b
          => (forall a. Data a => Value -> Either String a -> Either String a)
          -> Value
          -> Either String b
decodeFay specialCases value = specialCases value $
    parseDataOrTuple rec value
    `ext1R` parseArray rec value
    `extR` parseDouble value
    `extR` parseInt value
    `extR` parseBool value
    `extR` parseString value
    `extR` parseChar value
    `extR` parseText value
    `extR` parseUnit value
  where
    rec :: GenericParser
    rec = decodeFay specialCases

type GenericParser = Data a => Value -> Either String a

-- | Parse a data type or record or tuple.
parseDataOrTuple :: forall a. Data a => GenericParser -> Value -> Either String a
parseDataOrTuple rec value = result where
  result = getAndParse value
  typ = dataTypeOf (undefined :: a)
  getAndParse x =
    case x of
      Object obj -> parseObject rec typ obj
      Array tuple -> parseTuple rec typ tuple
      _ -> badData value

-- | Parse a tuple.
parseTuple :: Data a => GenericParser -> DataType -> Vector Value -> Either String a
parseTuple rec typ arr =
  case dataTypeConstrs typ of
    [cons] -> evalStateT (fromConstrM (do i:next <- get
                                          put next
                                          value <- lift (Vector.indexM arr i)
                                          lift (rec value))
                                      cons)
                         [0..]
    _ -> badData (Array arr)

-- | Parse a data constructor from an object.
parseObject :: Data a => GenericParser -> DataType -> HashMap Text Value -> Either String a
parseObject rec typ obj =
  case Map.lookup (Text.pack "instance") obj of
    Just (parseString -> Right name) ->
      case filter (\con -> showConstr con == name) (dataTypeConstrs typ) of
        [con] ->
          let fields = constrFields con
           in if null fields
                then makeSimple rec obj con
                else makeRecord rec obj con fields
        _ -> badData (Object obj)
    _ -> badData (Object obj)

-- | Make a simple ADT constructor from an object: { "slot1": 1, "slot2": 2} -> Foo 1 2
makeSimple :: Data a => GenericParser -> HashMap Text Value -> Constr -> Either String a
makeSimple rec obj cons =
  evalStateT (fromConstrM (do i:next <- get
                              put next
                              value <- lift (lookupField obj (Text.pack ("slot" ++ show i)))
                              lift (rec value))
                          cons)
             [1..]

-- | Make a record from a key-value: { "x": 1 } -> Foo { x = 1 }
makeRecord :: Data a => GenericParser -> HashMap Text Value -> Constr -> [String] -> Either String a
makeRecord rec obj cons fields =
  evalStateT (fromConstrM (do key:next <- get
                              put next
                              value <- lift (lookupField obj (Text.pack key))
                              lift (rec value))
                          cons)
             fields

lookupField :: HashMap Text Value -> Text -> Either String Value
lookupField obj key =
  justRight ("Missing field " ++ Text.unpack key ++ " in " ++ show (Object obj)) $
  Map.lookup key obj

-- | Parse a double.
parseDouble :: Value -> Either String Double
parseDouble = parseEither parseJSON

-- | Parse an int.
parseInt :: Value -> Either String Int
parseInt = parseEither parseJSON

-- | Parse a bool.
parseBool :: Value -> Either String Bool
parseBool value = case value of
  Bool n -> return n
  _ -> badData value

-- | Parse a string.
parseString :: Value -> Either String String
parseString value = case value of
  String s -> return (Text.unpack s)
  _ -> badData value

-- | Parse a char.
parseChar :: Value -> Either String Char
parseChar value = case value of
  String s | Just (c,_) <- Text.uncons s -> return c
  _ -> badData value

-- | Parse a Text.
parseText :: Value -> Either String Text
parseText value = case value of
  String s -> return s
  _ -> badData value

-- | Parse an array.
parseArray :: Data a => GenericParser -> Value -> Either String [a]
parseArray rec value = case value of
  Array xs -> mapM rec (Vector.toList xs)
  _ -> badData value

-- | Parse unit.
parseUnit :: Value -> Either String ()
parseUnit value = case value of
  Null -> return ()
  _ -> badData value

badData :: forall a. Data a => Value -> Either String a
badData value = Left $
  "Bad data in decodeFay - expected valid " ++
  show (typeOf (undefined :: a)) ++
  ", but got:\n" ++
  show value

justRight :: b -> Maybe a -> Either b a
justRight x Nothing = Left x
justRight _ (Just y) = Right y
