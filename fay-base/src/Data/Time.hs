{-# OPTIONS -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}

-- | A limited subset of the time package.

module Data.Time
  (-- * Compatible with the time package
  getCurrentTime
  ,fromGregorian
  ,UTCTime
  ,Day
  ,utctDay
  -- * Incompatible Fay-specific helpers
  ,showTime
  ,showDay)
  where

import Data.Data
import Data.Text
import FFI
import Prelude (Show,Eq,Ord,Int)

-- | Date representation (internally represented as milliseconds from Epoch).
data UTCTime
    deriving (Typeable)

-- We provide no methods, this is just to satisfy type-safety. No
-- methods work in Fay anyway.
instance Data UTCTime
instance Show UTCTime
instance Eq UTCTime
instance Ord UTCTime

-- | Day representation (internally represented as milliseconds from Epoch).
data Day
    deriving (Typeable)
-- We provide no methods, this is just to satisfy type-safety. No
-- methods work in Fay anyway.
instance Data Day
instance Show Day
instance Eq Day
instance Ord Day

-- | Get the current time.
getCurrentTime :: Fay UTCTime
getCurrentTime = ffi "(new Date()).getTime()"

-- | Convert from proleptic Gregorian calendar. First argument is
-- year, second month number (1-12), third day (1-31).
fromGregorian :: Int -- ^ Year.
              -> Int -- ^ Month.
              -> Int -- ^ Day.
              -> Day
fromGregorian = ffi "Date.UTC(%1,%2-1,%3)"

-- | Extract the day from the time.
utctDay :: UTCTime -> Day
utctDay = ffi "%1"

-- | Show a time. Meant for debugging purposes, not production presentation.
showTime :: UTCTime -> Text
showTime = ffi "new Date(%1).toString()"

-- | Show a day. Meant for debugging purposes, not production presentation.
showDay :: Day -> Text
showDay =
  ffi "date.getUTCFullYear() + ' ' + showMonth(date) + ' ' + (date.getUTCDate() + 1)"
