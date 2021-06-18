-- | This module provides data definitions and functions for date values.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haspara.Internal.Date where

import qualified Data.Aeson     as Aeson
import           Data.Bifunctor (first)
import           Data.Hashable  (Hashable(..))
import qualified Data.Text      as T
import qualified Data.Time      as DT


-- * Data Definition
-- &dataDefinition


-- | Type encoding for date values.
--
-- This is a convenience wrapper around 'Day' type. It helps us to avoid
-- defining orphan instances.
newtype Date = MkDate DT.Day deriving (Eq, Enum, Ord)


-- | 'Hashable' instance for 'Date'.
instance Hashable Date where
  hashWithSalt salt (MkDate (DT.ModifiedJulianDay i)) = hashWithSalt salt i


-- | 'Read' instance for 'Date'.
--
-- >>> read "2021-01-01" :: Date
-- 2021-01-01
-- >>> read "Just 2021-01-01" :: Maybe Date
-- Just 2021-01-01
instance Read Date where
  readsPrec _ = readParen False $ fmap (first MkDate) <$> DT.readSTime True DT.defaultTimeLocale "%Y-%m-%d"


-- | 'Show' instance for 'Date'.
--
-- >>> fromYMD 2020 12 31
-- 2020-12-31
instance Show Date where
  show = toString


-- | 'Aeson.FromJSON' instance for 'Date'.
--
-- >>> Aeson.decode "\"2020-12-31\"" :: Maybe Date
-- Just 2020-12-31
instance Aeson.FromJSON Date where
  parseJSON = Aeson.withText "Date" fromText


-- | 'Aeson.ToJSON' instance for 'Date'.
--
-- >>> Aeson.encode (MkDate (read "2021-01-01"))
-- "\"2021-01-01\""
instance Aeson.ToJSON Date where
  toJSON = Aeson.String . T.pack . show


-- * Constructors
-- &constructors


-- | Builds a 'Date' from a given 'Day'.
--
-- >>> fromDay (read "2021-01-01")
-- 2021-01-01
fromDay :: DT.Day -> Date
fromDay = MkDate


-- | Builds a 'Date' from a given year, month and day as in Gregorian calendar.
--
-- >>> fromYMD 2021 1 1
-- 2021-01-01
fromYMD :: Integer -> Int -> Int -> Date
fromYMD y m d = fromDay (DT.fromGregorian y m d)


-- | Attempts to parse and return 'Date' from a given 'String' with ISO format.
--
-- >>> fromString "2021-01-01" :: Maybe Date
-- Just 2021-01-01
-- >>> fromString "20210101" :: Maybe Date
-- Nothing
fromString :: MonadFail m => String -> m Date
fromString = fromFormattedString "%Y-%m-%d"


-- | Attempts to parse and return 'Date' from a given 'String' with given date format.
--
-- >>> fromFormattedString "%Y-%m-%d" "2021-01-01" :: Maybe Date
-- Just 2021-01-01
-- >>> fromFormattedString "%Y%m%d" "20210101" :: Maybe Date
-- Just 2021-01-01
-- >>> fromFormattedString "%Y%m%d" "202101" :: Maybe Date
-- Nothing
fromFormattedString :: MonadFail m => String -> String -> m Date
fromFormattedString fmt = fmap fromDay . DT.parseTimeM False DT.defaultTimeLocale fmt


-- | Attempts to parse and return 'Date' from a given 'T.Text' with ISO format.
--
-- >>> fromText "2021-01-01" :: Maybe Date
-- Just 2021-01-01
-- >>> fromText "20210101" :: Maybe Date
-- Nothing
fromText :: MonadFail m => T.Text -> m Date
fromText = fromString . T.unpack


-- | Attempts to parse and return 'Date' from a given 'T.Text' with ISO format.
--
-- >>> fromFormattedText "%Y-%m-%d" "2021-01-01" :: Maybe Date
-- Just 2021-01-01
-- >>> fromFormattedText "%Y%m%d" "20210101" :: Maybe Date
-- Just 2021-01-01
-- >>> fromFormattedText "%Y%m%d" "202101" :: Maybe Date
-- Nothing
fromFormattedText :: MonadFail m => String -> T.Text -> m Date
fromFormattedText fmt = fromFormattedString fmt  . T.unpack


-- * Conversions
-- &conversions


-- | Converts 'Date' value to a 'DT.Day' value.
--
-- >>> toDay (read "2021-01-01")
-- 2021-01-01
toDay :: Date -> DT.Day
toDay (MkDate d) = d


-- | Converts 'Date' value to a 3-tuple of year, month and day.
--
-- >>> toYMD (read "2020-12-31")
-- (2020,12,31)
toYMD :: Date -> (Integer, Int, Int)
toYMD = DT.toGregorian . toDay


-- | Converts 'Date' value into a 'String' value with ISO format.
--
-- >>> toString (read "2021-01-01")
-- "2021-01-01"
toString :: Date -> String
toString = toFormattedString "%Y-%m-%d"


-- | Converts 'Date' value into a 'String' value with the given format.
--
-- >>> toFormattedString "%Y-%m-%d" (read "2021-01-01")
-- "2021-01-01"
-- >>> toFormattedString "%d/%m/%Y" (read "2021-01-01")
-- "01/01/2021"
toFormattedString :: String -> Date -> String
toFormattedString fmt = DT.formatTime DT.defaultTimeLocale fmt . toDay


-- | Converts 'Date' value into a 'T.Text' value with ISO format.
--
-- >>> toText (read "2021-01-01")
-- "2021-01-01"
toText :: Date -> T.Text
toText = T.pack . toString


-- | Converts 'Date' value into a 'T.Text' value with the given format.
--
-- >>> toFormattedText "%Y-%m-%d" (read "2021-01-01")
-- "2021-01-01"
-- >>> toFormattedText "%d/%m/%Y" (read "2021-01-01")
-- "01/01/2021"
toFormattedText :: String -> Date -> T.Text
toFormattedText fmt = T.pack . toFormattedString fmt


-- * Helper Functions
-- &helpers


-- | Adds (or subtracts) some days.
--
-- >>> addDays (-1) $ fromYMD 2021 1 1
-- 2020-12-31
-- >>> addDays 1 $ addDays (-1) $ fromYMD 2021 1 1
-- 2021-01-01
addDays :: Integer -> Date -> Date
addDays x (MkDate d) = MkDate (DT.addDays x d)
