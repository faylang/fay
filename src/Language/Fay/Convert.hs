{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-type-defaults #-}

-- | Convert a Haskell value to a (JSON representation of a) Fay value.

module Language.Fay.Convert
  (showToFay
  ,readFromFay
  ,runShowToFayTests
  ,runReadFromFayTests)
  where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad

import           Data.Aeson
import           Data.Attoparsec.Number
import qualified Data.ByteString.Lazy as Bytes
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.Char
import           Data.Data
import           Data.Function

import           Data.List

import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import           Data.Ratio
import qualified Data.Text           as Text
import qualified Data.Vector         as Vector
import           Numeric
import           Safe
import           Test.HUnit
import qualified Text.Show.Pretty    as Show

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
        readRecord name obj <|> readData name obj
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
    fields <- forM (filter ((/="instance").fst) (Map.toList obj)) $ \(_,v) -> do
      cvalue <- convert v
      return cvalue
    return (intercalate " " (Text.unpack name : fields))

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
      return (intercalate " " [key,"=",cvalue])
    guard $ not $ null fields
    return (Text.unpack name ++
            if null fields
               then ""
               else " {" ++ intercalate ", " fields ++ "}")

  typ = dataTypeOf $ resType result
  resType :: Maybe a -> a
  resType = undefined

--------------------------------------------------------------------------------
-- Test cases

-- | A test.
data Testcase = forall x. Show x => Testcase x Bytes.ByteString

-- | A read test.
data ReadTest = forall x. (Data x,Show x,Eq x,Read x) => ReadTest x

-- | Run the tests.
runShowToFayTests :: Bool -> IO ()
runShowToFayTests ghci = runThoseTests makeTests where
  makeTests = TestList $ flip map showTests $ \(Testcase value output) ->
    let label = show value
    in TestLabel label $ TestCase $
         assertEqual label output (encode (showToFay value))
  putter = PutText (\str _bool _st -> putStrLn str) ()
  runThoseTests ts | ghci = fmap fst (runTestText putter ts) >>= putStrLn . showCounts
                   | otherwise = void (runTestTT ts)

-- | Run the tests.
runReadFromFayTests :: Bool -> IO ()
runReadFromFayTests ghci = runThoseTests makeTests where
  makeTests = TestList $ flip map readTests $ \(ReadTest value) ->
    let label = show value
    in TestLabel label $ TestCase $
         assertEqual label (Just value) (showToFay value >>= readFromFay)
  putter = PutText (\str _bool _st -> putStrLn str) ()
  runThoseTests ts | ghci = fmap fst (runTestText putter ts) >>= putStrLn . showCounts
                   | otherwise = void (runTestTT ts)

--------------------------------------------------------------------------------
-- Tests

-- | Read tests.
readTests :: [ReadTest]
readTests =
  [ReadTest $ NullaryConstructor
  ,ReadTest $ NAryConstructor 123 66.6
  ,ReadTest $ LabelledRecord { barInt = 123, barDouble = 66.6 }
  ,ReadTest $ LabelledRecord2 { bar = 123, bob = 66.6 }
  ]

-- | Test cases.
showTests :: [Testcase]
showTests =
   -- Fundamental data types
  [(1 :: Int) → "1"
  ,(1 :: Double) → "1.0"
  ,(1/2 :: Double) → "0.5"
  ,(1%2 :: Rational) → "0.5"
  ,([1,2] :: [Int]) → "[1,2]"
  ,((1,2) :: (Int,Int)) → "[1,2]"
  ,"abc" → "\"abc\""
  ,'a' → "\"a\""
  -- Data records
  ,NullaryConstructor → "{\"instance\":\"NullaryConstructor\"}"
  ,NAryConstructor 123 4.5 → "{\"slot1\":123,\"slot2\":4.5,\"instance\":\"NAryConstructor\"}"
  ,LabelledRecord { barInt = 123, barDouble = 4.5 }
     → "{\"barDouble\":4.5,\"barInt\":123,\"instance\":\"LabelledRecord\"}"
  -- Unicode
  ,"¡ ¢ £ ¤ ¥ " → "\"¡ ¢ £ ¤ ¥ \""
  ,"Ā ā Ă ă Ą " → "\"Ā ā Ă ă Ą \""
  ,"ƀ Ɓ Ƃ ƃ Ƅ " → "\"ƀ Ɓ Ƃ ƃ Ƅ \""
  ,"ɐ ɑ ɒ ɓ ɔ " → "\"ɐ ɑ ɒ ɓ ɔ \""
  ,"Ё Ђ Ѓ Є Ѕ " → "\"Ё Ђ Ѓ Є Ѕ \""
  ,"Ա Բ Գ Դ Ե " → "\"Ա Բ Գ Դ Ե \""
  ,"، ؛ ؟ ء آ " → "\"، ؛ ؟ ء آ \""
  ,"ँ ं ः अ आ " → "\"ँ ं ः अ आ \""
  ,"ఁ ం ః అ ఆ " → "\"ఁ ం ః అ ఆ \""
  ,"ก ข ฃ ค ฅ " → "\"ก ข ฃ ค ฅ \""
  ,"ກ ຂ ຄ ງ ຈ " → "\"ກ ຂ ຄ ງ ຈ \""
  ,"ༀ ༁ ༂ ༃ ༄ " → "\"ༀ ༁ ༂ ༃ ༄ \""
  ,"Ⴀ Ⴁ Ⴂ Ⴃ Ⴄ " → "\"Ⴀ Ⴁ Ⴂ Ⴃ Ⴄ \""
  ,"Ḁ ḁ Ḃ ḃ Ḅ " → "\"Ḁ ḁ Ḃ ḃ Ḅ \""
  ,"ぁ あ ぃ い ぅ " → "\"ぁ あ ぃ い ぅ \""
  ,"ァ ア ィ イ ゥ " → "\"ァ ア ィ イ ゥ \""
  ,"ㄅ ㄆ ㄇ ㄈ ㄉ " → "\"ㄅ ㄆ ㄇ ㄈ ㄉ \""
  ,"ㄱ ㄲ ㄳ ㄴ ㄵ " → "\"ㄱ ㄲ ㄳ ㄴ ㄵ \""
  ]

  where x → y = Testcase x (UTF8.fromString y)

-- | Nullary constructor.
data NullaryConstructor = NullaryConstructor
  deriving (Show,Data,Typeable,Read,Eq)

-- | n-ary labelless constructor.
data NAryConstructor = NAryConstructor Int Double
  deriving (Show,Data,Typeable,Read,Eq)

-- | Labelled record.
data LabelledRecord = LabelledRecord { barInt :: Int, barDouble :: Double }
                    | LabelledRecord2 { bar :: Int, bob :: Double }
  deriving (Show,Data,Typeable,Read,Eq)
