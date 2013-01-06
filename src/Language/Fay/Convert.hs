{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}
{-# OPTIONS -fno-warn-type-defaults #-}

-- | Convert a Haskell value to a (JSON representation of a) Fay value.

module Language.Fay.Convert
  (showToFay
  ,readFromFay)
  where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Aeson
import           Data.Attoparsec.Number
import           Data.Char
import           Data.Data
import           Data.Function
import           Data.Generics.Aliases
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as Map
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Vector            as Vector
import           Numeric
import           Safe
import qualified Text.Show.Pretty       as Show

--------------------------------------------------------------------------------
-- The conversion functions.

-- | Convert a Haskell value to a value representing a Fay value.
showToFay :: Show a => a -> Maybe Value
showToFay = Show.reify >=> convert where
  convert value = case value of
    -- Special cases
    Show.Con "True" _    -> return (Bool True)
    Show.Con "False" _   -> return (Bool False)

    -- Objects/records
    Show.Con name values -> fmap (Object . Map.fromList . (("instance",string name) :))
                                 (slots values)
    Show.Rec name fields -> fmap (Object . Map.fromList . (("instance",string name) :))
                                 (mapM (uncurry keyval) fields)

    -- List types
    Show.Tuple values -> fmap (Array . Vector.fromList) (mapM convert values)
    Show.List values  -> fmap (Array . Vector.fromList) (mapM convert values)

    -- Text types
    Show.String chars -> fmap string (readMay chars)
    Show.Char char    -> fmap (string.return) (readMay char)

    -- Numeric types (everything treated as a double)
    Show.Neg{}     -> double <|> int
    Show.Integer{} -> int
    Show.Float{}   -> double
    Show.Ratio{}   -> double
    where double = convertDouble value
          int = convertInt value

  -- Number converters
  convertDouble = fmap (Number . D) . pDouble
  convertInt = fmap (Number . I) . pInt

  -- Number parsers
  pDouble :: Show.Value -> Maybe Double
  pDouble value = case value of
    Show.Float str   -> getDouble str
    Show.Ratio x y   -> liftM2 (on (/) fromIntegral) (pInt x) (pInt y)
    Show.Neg str     -> fmap (* (-1)) (pDouble str)
    _ -> Nothing
  pInt value = case value of
    Show.Integer str -> getInt str
    Show.Neg str     -> fmap (* (-1)) (pInt str)
    _ -> Nothing

  -- Number readers
  getDouble :: String -> Maybe Double
  getDouble = fmap fst . listToMaybe . readFloat
  getInt :: String -> Maybe Integer
  getInt = fmap fst . listToMaybe . readInt 10 isDigit charToInt
    where charToInt c = fromEnum c - fromEnum '0'

  -- Utilities
  string = String . Text.pack
  slots = zipWithM keyval (map (("slot"++).show) [1::Int ..])
  keyval key val = fmap (Text.pack key,) (convert val)

-- | Convert a value representing a Fay value to a Haskell value.

readFromFay :: Data a => Value -> Maybe a
readFromFay value = do
  parseData value
  `ext1R` parseArray value
  `extR` parseDouble value
  `extR` parseInt value
  `extR` parseBool value
  `extR` parseString value
  `extR` parseText value

-- | Parse a data type or record.
parseData :: Data a => Value -> Maybe a
parseData value = result where
  result = getObject value >>= parseObject typ
  typ = dataTypeOf (fromJust result)
  getObject x =
    case x of
      Object obj -> return obj
      _ -> mzero

-- | Parse a data constructor from an object.
parseObject :: Data a => DataType -> HashMap Text Value -> Maybe a
parseObject typ obj = listToMaybe (catMaybes choices) where
  choices = map makeConstructor constructors
  constructors = dataTypeConstrs typ
  makeConstructor cons = do
    name <- Map.lookup (Text.pack "instance") obj >>= parseString
    guard (showConstr cons == name)
    if null fields
      then makeSimple obj cons
      else makeRecord obj cons fields

      where fields = constrFields cons

-- | Make a simple ADT constructor from an object: { "slot1": 1, "slot2": 2} -> Foo 1 2
makeSimple :: Data a => HashMap Text Value -> Constr -> Maybe a
makeSimple obj cons =
  evalStateT (fromConstrM (do i:next <- get
                              put next
                              value <- lift (Map.lookup (Text.pack ("slot" ++ show i)) obj)
                              lift (readFromFay value))
                          cons)
             [1..]

-- | Make a record from a key-value: { "x": 1 } -> Foo { x = 1 }
makeRecord :: Data a => HashMap Text Value -> Constr -> [String] -> Maybe a
makeRecord obj cons fields =
  evalStateT (fromConstrM (do key:next <- get
                              put next
                              value <- lift (Map.lookup (Text.pack key) obj)
                              lift (readFromFay value))
                          cons)
             fields

-- | Parse a double.
parseDouble :: Value -> Maybe Double
parseDouble value = do
  number <- parseNumber value
  case number of
    I n -> return (fromIntegral n)
    D n -> return n

-- | Parse an int.
parseInt :: Value -> Maybe Int
parseInt value = do
  number <- parseNumber value
  case number of
    I n -> return (fromIntegral n)
    _ -> mzero

-- | Parse a number.
parseNumber :: Value -> Maybe Number
parseNumber value =
  case value of
    Number n -> return n
    _ -> mzero

-- | Parse a bool.
parseBool :: Value -> Maybe Bool
parseBool value =
  case value of
    Bool n -> return n
    _ -> mzero

-- | Parse a string.
parseString :: Value -> Maybe String
parseString value =
  case value of
    String s -> return (Text.unpack s)
    _ -> mzero

-- | Parse a Text.
parseText :: Value -> Maybe Text
parseText value =
  case value of
    String s -> return s
    _ -> mzero

-- | Parse an array.
parseArray :: Data a => Value -> Maybe [a]
parseArray value =
  case value of
    Array xs -> mapM readFromFay (Vector.toList xs)
    _ -> mzero
