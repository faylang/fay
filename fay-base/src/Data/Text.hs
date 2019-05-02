{-# OPTIONS -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Compatible API with the `text' package.

module Data.Text
  ( Text
  -- * Creation and elimination
  , pack
  , unpack
  , fromString
  , empty
  -- * Conversions
  , showInt
  , toShortest
  -- * I/O
  , putStrLn
  -- * Breaking into many substrings
  , splitOn
  , stripSuffix
  -- * Basic interface
  , cons
  , snoc
  , append
  , (<>)
  , uncons
  , head
  , init
  , last
  , tail
  , null
  , length
  -- * Special folds
  , maximum
  , all
  , any
  , concatMap
  , concat
  , minimum
  -- * Case conversion
  , toLower
  , toUpper
  -- * Transformations
  , map
  , intercalate
  , intersperse
  , reverse
  -- * Predicates
  , isPrefixOf
  -- * Substrings
  , drop
  , take
  -- * Breaking into lines and words
  , unlines
  , lines
  ) where

import Data.Data
import FFI
import Data.Nullable (fromNullable)
import Prelude (Eq,String,Int,Bool,Char,Maybe,Double,Ord,Show,error)
import qualified "base" Data.String as B (IsString (..))

-- | A space efficient, packed, unboxed Unicode text type.
data Text
deriving instance Eq Text
deriving instance Data Text
deriving instance Typeable Text
deriving instance Show Text
instance Ord Text
instance B.IsString Text where fromString = error "the method fromString can never be called"

-- | O(n) The intercalate function takes a Text and a list of Texts and
-- concatenates the list after interspersing the first argument
-- between each element of the list.
intercalate :: Text -> [Text] -> Text
intercalate = ffi "%2.join(%1)"

-- | Convert from a string to text.
fromString :: String -> Text
fromString = ffi "%1"

-- | O(n) Adds a character to the end of a Text. This copies the
-- entire array in the process, unless fused. Subject to
-- fusion. Performs replacement on invalid scalar values.
snoc :: Text -> Char -> Text
snoc = ffi "%1 + %2"

-- | O(n) Adds a character to the front of a Text. This function is
-- more costly than its List counterpart because it requires copying a
-- new array. Subject to fusion. Performs replacement on invalid
-- scalar values.
cons :: Char -> Text -> Text
cons = ffi "%1 + %2"

-- | O(n) Convert a String into a Text. Subject to fusion. Performs
-- replacement on invalid scalar values.
pack :: String -> Text
pack = ffi "%1"

-- | O(n) Convert a Text into a String. Subject to fusion.
unpack :: Text -> String
unpack = ffi "%1"

-- | O(n) Appends one Text to the other by copying both of them into a
-- new Text. Subject to fusion.
append :: Text -> Text -> Text
append = ffi "%1 + %2"

-- | Append two texts.
(<>) :: Text -> Text -> Text
(<>) = ffi "%1 + %2"

-- | O(n) Returns the number of characters in a Text. Subject to
-- fusion.
length :: Text -> Int
length = ffi "%1.length"

-- | O(1) Tests whether a Text is empty or not. Subject to fusion.
null :: Text -> Bool
null = ffi "%1.length == 0"

-- | O(n) take n, applied to a Text, returns the prefix of the Text of
-- length n, or the Text itself if n is greater than the length of the
-- Text. Subject to fusion.
take :: Int -> Text -> Text
take = ffi "%2.substring(0,%1)"

-- | O(n) drop n, applied to a Text, returns the suffix of the Text
-- after the first n characters, or the empty Text if n is greater
-- than the length of the Text. Subject to fusion.
drop :: Int -> Text -> Text
drop = ffi "%2.substring(%1)"

-- | O(1) The empty Text.

-- Basic interface
empty :: Text
empty = ffi "\"\""

-- | O(n) Breaks a Text up into a list of Texts at newline Chars. The
-- resulting strings do not contain newlines.
lines :: Text -> [Text]
lines = ffi "%1.split('\\n')"

-- | O(n) Joins lines, after appending a terminating newline to each.
unlines :: [Text] -> Text
unlines = ffi "%1.join('\\n')"

-- | O(n) The isPrefixOf function takes two Texts and returns True iff
-- the first is a prefix of the second. Subject to fusion.
-- http://docs.closure-library.googlecode.com/git/closure_goog_string_string.js.source.html
isPrefixOf :: Text -> Text -> Bool
isPrefixOf = ffi "%2.lastIndexOf(%1, 0) == 0"

-- | O(n) The intersperse function takes a character and places it
-- between the characters of a Text.  Subject to fusion. Performs
-- replacement on invalid scalar values.
intersperse :: Char -> Text -> Text
intersperse = ffi "%2.split('').join(%1)"

-- | O(n) Reverse the characters of a string. Subject to fusion.
reverse :: Text -> Text
reverse = ffi "%1.split('').reverse().join('')"

-- | O(n) Return the prefix of the second string if its suffix matches
-- the entire first string.
stripSuffix :: Text -- ^ Suffix.
                -> Text -- ^ Text.
                -> Maybe Text
stripSuffix prefix text =
  fromNullable (extract prefix text)
  where extract :: Text -> Text -> Nullable Text
        extract =
          ffi "(function(suffix,text){ return text.substring(text.length - suffix.length) == suffix? text.substring(0,text.length - suffix.length) : null; })(%1,%2)"

-- | O(m+n) Break a Text into pieces separated by the first Text
-- argument, consuming the delimiter. An empty delimiter is
-- invalid, and will cause an error to be raised.
splitOn :: Text -> Text -> [Text]
splitOn = ffi "%2.split(%1)"

-- |
putStrLn :: Text -> Fay ()
putStrLn = ffi "console.log('%%s',%1)"

-- |
toShortest :: Double -> Text
toShortest = ffi "%1.toString()"

-- |
showInt :: Int -> Text
showInt = ffi "%1.toString()"

-- | O(1) Returns the first character and rest of a Text, or Nothing
-- if empty. Subject to fusion.
uncons :: Text -> Maybe (Char, Text)
uncons = ffi "%1[0] ? { instance: 'Just', slot1 : [%1[0],%1.slice(1)] } : { instance : 'Nothing' }"

-- | O(1) Returns the first character of a Text, which must be
-- non-empty. Subject to fusion.
head :: Text -> Char
head = ffi "%1[0] || (function () {throw new Error('Data.Text.head: empty Text'); }())"

-- | O(1) Returns the last character of a Text, which must be
-- non-empty. Subject to fusion.
last :: Text -> Char
last = ffi "%1.length ? %1[%1.length-1] : (function() { throw new Error('Data.Text.last: empty Text') })()"

-- | O(1) Returns all characters after the head of a Text, which must
-- be non-empty. Subject to fusion.
tail :: Text -> Text
tail = ffi "%1.length ? %1.slice(1) : (function () { throw new Error('Data.Text.tail: empty Text') })()"

-- | O(1) Returns all but the last character of a Text, which must be
-- non-empty. Subject to fusion.
init :: Text -> Text
init = ffi "%1.length ? %1.slice(0,-1) : (function () { throw new Error('Data.Text.init: empty Text') })()"

-- | O(n) map f t is the Text obtained by applying f to each element
-- of t. Subject to fusion. Performs replacement on invalid scalar
-- values.
map :: (Char -> Char) -> Text -> Text
map = ffi "[].map.call(%2, %1).join('')"

-- | O(n) Convert a string to lower case, using simple case
-- conversion. The result string may be longer than the input
-- string. For instance, "İ" (Latin capital letter I with dot above,
-- U+0130) maps to the sequence "i" (Latin small letter i, U+0069)
-- followed by " ̇" (combining dot above, U+0307).
toLower :: Text -> Text
toLower = ffi "%1.toLowerCase()"

-- | O(n) Convert a string to upper case, using simple case
-- conversion. The result string may be longer than the input
-- string. For instance, the German "ß" (eszett, U+00DF) maps to the
-- two-letter sequence "SS".
toUpper :: Text -> Text
toUpper = ffi "%1.toUpperCase()"

-- | O(n) Concatenate a list of Texts.
concat :: [Text] -> Text
concat = ffi "%1.join('')"

-- | O(n) Map a function over a Text that results in a Text, and
-- concatenate the results.
concatMap :: (Char -> Text) -> Text -> Text
concatMap = ffi "[].map.call(%2, %1).join('')"

-- | O(n) any p t determines whether any character in the Text t
-- satisifes the predicate p. Subject to fusion.
any :: (Char -> Bool) -> Text -> Bool
any = ffi "[].filter.call(%2, %1).length > 0"

-- | O(n) all p t determines whether all characters in the Text t
-- satisify the predicate p. Subject to fusion.
all :: (Char -> Bool) -> Text -> Bool
all = ffi "[].filter.call(%2, %1).length == %1.length"

-- | O(n) maximum returns the maximum value from a Text, which must be
-- non-empty. Subject to fusion.
maximum :: Text -> Char
maximum = ffi "(function (s) { \
  \   if (s === '') { throw new Error('Data.Text.maximum: empty string'); } \
  \   var max = s[0]; \
  \   for (var i = 1; i < s.length; s++) { \
  \     if (s[i] > max) { max = s[i]; } \
  \   } \
  \   return max; \
  \ })(%1)"

-- | O(n) minimum returns the minimum value from a Text, which must be
-- non-empty. Subject to fusion.
minimum :: Text -> Char
minimum = ffi "(function (s) { \
  \   if (s === '') { throw new Error('Data.Text.maximum: empty string'); } \
  \   var min = s[0]; \
  \   for (var i = 1; i < s.length; s++) { \
  \     if (s[i] < min) { min = s[i]; } \
  \   } \
  \   return min; \
  \ })(%1)"
