{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS -fno-warn-type-defaults #-}

-- | Convert a Haskell value to a (JSON representation of a) Fay value.

module Language.Fay.Convert
  (showToFay
  ,readFromFay)
  where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Aeson
import           Data.Attoparsec.Number

import           Data.Char
import           Data.Data
import           Data.Function
import qualified Data.HashMap.Strict    as Map
import           Data.List
import           Data.Maybe
import           Data.Ord
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
  convertDouble = fmap (Number . D) . parseDouble
  convertInt = fmap (Number . I) . parseInt

  -- Number parsers
  parseDouble :: Show.Value -> Maybe Double
  parseDouble value = case value of
    Show.Float str   -> getDouble str
    Show.Ratio x y   -> liftM2 (on (/) fromIntegral) (parseInt x) (parseInt y)
    Show.Neg str     -> fmap (* (-1)) (parseDouble str)
    _ -> Nothing
  parseInt value = case value of
    Show.Integer str -> getInt str
    Show.Neg str     -> fmap (* (-1)) (parseInt str)
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
readFromFay :: (Data a,Read a) => Value -> Maybe a
readFromFay value = result where
  result = (convert >=> readMay) value
  convert v =
    case v of
      Object obj -> do
        name <- Map.lookup "instance" obj >>= getText
        fmap parens (readRecord name obj <|> readData name obj)
      Array array -> do
        elems <- mapM convert (Vector.toList array)
        return $ concat ["[",intercalate "," elems,"]"]
      String str -> return (show str)
      Number num -> return $ case num of
        I integer -> show integer
        D double -> show double
      Bool bool -> return $ show bool
      Null -> Nothing

  getText i = case i of
    String s -> return s
    _ -> Nothing

  readData name obj = do
    fields <- forM assocs $ \(_,v) -> do
      cvalue <- convert v
      return cvalue
    return (intercalate " " (Text.unpack name : fields))
      where assocs = sortBy (comparing fst)
                            (filter ((/="instance").fst) (Map.toList obj))

  readRecord name (Map.toList -> assocs) = go (dataTypeConstrs typ)
    where go (cons:conses) =
            readConstructor name assocs cons <|> go conses
          go [] = Nothing

  readConstructor name assocs cons = do
    let getField key =
          case lookup key (map (first Text.unpack) assocs) of
            Just v  -> return (key,v)
            Nothing -> Nothing
    fields <- forM (constrFields cons) $ \field -> do
      (key,v) <- getField field
      cvalue <- convert v
      return (unwords [key,"=",cvalue])
    guard $ not $ null fields
    return (Text.unpack name ++
            if null fields
               then ""
               else " {" ++ intercalate ", " fields ++ "}")

  typ = dataTypeOf $ resType result
  resType :: Maybe a -> a
  resType = undefined
  parens x = "(" ++ x ++ ")"
