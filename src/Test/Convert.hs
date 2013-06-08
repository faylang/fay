{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}

module Test.Convert (tests) where

import qualified Data.Aeson.Parser              as Aeson
import           Data.Attoparsec.ByteString
import qualified Data.ByteString                as Bytes
import qualified Data.ByteString.UTF8           as UTF8
import           Data.Data
import           Data.Ratio
import           Data.Text                      (Text, pack)
import           Fay.Convert
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     (assertEqual)

tests :: Test
tests = testGroup "Test.Convert" [reading, showing]
  where reading = testGroup "reading" $
          flip map readTests $ \(ReadTest value) ->
            let label = show value
            in testCase label $
                 assertEqual label (Just value) (showToFay value >>= readFromFay)
        showing = testGroup "showing" $
          flip map showTests $ \(Testcase value output) ->
            let label = show value
            in testCase label $
                 assertEqual label
                   (either error Just $ parseOnly Aeson.value output)
                   (showToFay value)


--------------------------------------------------------------------------------
-- Test cases

-- | A test.
data Testcase = forall x. Show x => Testcase x Bytes.ByteString

-- | A read test.
data ReadTest = forall x. (Data x,Show x,Eq x,Read x) => ReadTest x


-- | Tests

-- | Read tests.
readTests :: [ReadTest]
readTests =
  [ReadTest $ NullaryConstructor
  ,ReadTest $ NAryConstructor 123 66.6
  ,ReadTest $ LabelledRecord { barInt = 123, barDouble = 66.6 }
  ,ReadTest $ LabelledRecord2 { bar = 123, bob = 66.6 }
  ,ReadTest $ FooBar "Tinkie Winkie" "Humanzee" Zot
  ,ReadTest $ Bar $ Foo "one" "two"
  ,ReadTest $ StepcutFoo 123
  ,ReadTest $ StepcutBar (StepcutFoo 456)
  ,ReadTest $ StepcutFoo' 789
  ,ReadTest $ Baz (StepcutFoo' 10112)
  ,ReadTest $ TextConstructor $ pack "This is \"some text\n\n\""
  ,ReadTest $ (("foo",'a') :: (String,Char))
  ,ReadTest $ ((pack "foo",'a',23) :: (Text,Char,Int))
  ,ReadTest $ TupleList [(pack "foo",pack "bar")]
  ,ReadTest $ TupleList' [((pack "foo",23) :: (Text,Int))]
  ,ReadTest $ ()
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
  , () → "null"
  -- Data records
  ,NullaryConstructor → "{\"instance\":\"NullaryConstructor\"}"
  ,NAryConstructor 123 4.5 → "{\"slot1\":123,\"slot2\":4.5,\"instance\":\"NAryConstructor\"}"
  ,LabelledRecord { barInt = 123, barDouble = 4.5 }
     → "{\"barDouble\":4.5,\"barInt\":123,\"instance\":\"LabelledRecord\"}"
  ,Bar (Foo "one" "two") → "{\"slot1\":{\"slot1\":\"one\",\"slot2\":\"two\",\"instance\":\"Foo\"},\"instance\":\"Bar\"}"
  ,TextConstructor (pack "foo bar baz") → "{\"slot1\":\"foo bar baz\",\"instance\":\"TextConstructor\"}"
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

data Foo = Foo String String
  deriving (Show,Data,Typeable,Read,Eq)
data Bar = Bar Foo
  deriving (Show,Data,Typeable,Read,Eq)

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

-- | Order matters in unlabelled constructors.
data SomeThing =
  FooBar String String Zot
  deriving (Read,Data,Typeable,Show,Eq)

-- | This triggers order difference. Go figure.
data Zot = Zot
  deriving (Read,Data,Typeable,Show,Eq)

data StepcutFoo = StepcutFoo { _unStepcutFoo :: Int }
    deriving (Eq, Show, Read, Typeable, Data)

data StepcutBar = StepcutBar StepcutFoo
    deriving (Eq, Show, Read, Typeable, Data)

data StepcutFoo' = StepcutFoo' Int
    deriving (Eq, Show, Read, Typeable, Data)

data Baz = Baz StepcutFoo'
    deriving (Eq, Show, Read, Typeable, Data)

data TextConstructor = TextConstructor Text
    deriving (Eq, Show, Read, Typeable, Data)

data TupleList = TupleList [(Text,Text)]
  deriving (Read, Typeable, Data, Show, Eq)

data TupleList' a = TupleList' [(Text,a)]
  deriving (Read, Typeable, Data, Show, Eq)
