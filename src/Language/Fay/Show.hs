{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Convert a Haskell value to a (JSON representation of a) Fay value.

module Language.Fay.Show
  (showToFay,runShowToFayTests)
  where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Attoparsec.Number
import qualified Data.ByteString.Lazy as Bytes
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import           Data.Char
import           Data.Function
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import           Data.Ratio
import qualified Data.Text           as Text
import qualified Data.Vector         as Vector
import           Numeric
import           Safe
import           Test.HUnit
import qualified Text.Show.Pretty    as Show

-- | Convert a Haskell value to a J.value representing a Fay value.
showToFay :: Show a => a -> Maybe Value
showToFay = Show.reify >=> convert where
  convert value = case value of
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

--------------------------------------------------------------------------------
-- Test cases

-- | A test.
data Testcase = forall x. Show x => Testcase x Bytes.ByteString

-- | Run the tests.
runShowToFayTests :: Bool -> IO ()
runShowToFayTests ghci = runThoseTests makeTests where
  makeTests = TestList $ flip map tests $ \(Testcase value output) ->
    let label = show value
    in TestLabel label $ TestCase $
         assertEqual label output (encode (showToFay value))
  putter = PutText (\str _bool st -> putStrLn str) ()
  runThoseTests tests | ghci = fmap fst (runTestText putter tests) >>= putStrLn . showCounts
                      | otherwise = void (runTestTT tests)

--------------------------------------------------------------------------------
-- Tests

-- | Test cases.
tests :: [Testcase]
tests =
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
  ,LabelledRecord 123 4.5 → "{\"barDouble\":4.5,\"barInt\":123,\"instance\":\"LabelledRecord\"}"
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
  deriving Show

-- | n-ary labelless constructor.
data NAryConstructor = NAryConstructor Int Double
  deriving Show

-- | Labelled record.
data LabelledRecord = LabelledRecord { barInt :: Int, barDouble :: Double }
  deriving Show
