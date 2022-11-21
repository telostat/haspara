{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides definitions for modeling and working with foreign
-- exchange (FX) rate quotations.
module Haspara.FxQuote where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as Aeson
import Data.Foldable (foldl')
import qualified Data.Map.Strict as SM
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Data.Time (Day, addDays)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat)
import Haspara.Currency (Currency, CurrencyPair (CurrencyPair))
import Haspara.Internal.Aeson (commonAesonOptions)
import Haspara.Quantity (Quantity (..), mkQuantity)
import Refined (Positive, Refined, refineError)


-- * FX Rate Quotation


-- | Type encoding for FX rate quotations with fixed precision.
--
-- An FX rate quotation is a 3-tuple of:
--
-- 1. a currency pair the rate is quoted for, and
-- 2. a date that the quotation is effective as of,
-- 3. a (positive) rate as the value of the quotation.
data FxQuote (s :: Nat) = MkFxQuote
  { fxQuotePair :: !CurrencyPair
  -- ^ Currency pair of the FX rate.
  , fxQuoteDate :: !Day
  -- ^ Actual date of the FX rate.
  , fxQuoteRate :: !(Refined Positive (Quantity s))
  -- ^ (Positive) rate value of the FX rate.
  }
  deriving (Eq, Generic, Ord, Show)


instance KnownNat s => Aeson.FromJSON (FxQuote s) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "fxQuote"


instance KnownNat s => Aeson.ToJSON (FxQuote s) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "fxQuote"
  toEncoding = Aeson.genericToEncoding $ commonAesonOptions "fxQuote"


-- | Smart constructor for 'FxQuote' values within @'MonadError' 'T.Text'@
-- context.
--
-- The rate is expected to be a positive value. If it is not, the function will
-- throw an error.
--
-- >>> :set -XTypeApplications
-- >>> mkFxQuoteError @(Either _) @2 "EUR" "USD" (read "2021-12-31") 1.16
-- Right (MkFxQuote {fxQuotePair = EUR/USD, fxQuoteDate = 2021-12-31, fxQuoteRate = Refined 1.16})
-- >>> mkFxQuoteError @(Either _) @2 "EUR" "USD" (read "2021-12-31") (-1.16)
-- Left "Can not create FX Rate. Error was:   The predicate (GreaterThan 0) failed with the message: Value is not greater than 0\n"
mkFxQuoteError
  :: MonadError T.Text m
  => KnownNat s
  => Currency
  -- ^ Base currency (from) of the FX quotation.
  -> Currency
  -- ^ Quote currency (to) of the FX quotation.
  -> Day
  -- ^ Date of the FX quotation.
  -> Scientific
  -- ^ FX quotation rate, expected to be positive.
  -> m (FxQuote s)
mkFxQuoteError ccy1 ccy2 date rate =
  either (throwError . (<>) "Can not create FX Rate. Error was: ") pure $ do
    pval <- either (Left . T.pack . show) pure $ refineError (mkQuantity rate)
    pure $ MkFxQuote (CurrencyPair ccy1 ccy2) date pval


-- | Smart constructor for 'FxQuote' values within 'MonadFail' context.
--
-- The rate is expected to be a positive value. If it is not, the function will
-- fail.
--
-- >>> :set -XTypeApplications
-- >>> mkFxQuoteFail @Maybe @2 "EUR" "USD" (read "2021-12-31") 1.16
-- Just (MkFxQuote {fxQuotePair = EUR/USD, fxQuoteDate = 2021-12-31, fxQuoteRate = Refined 1.16})
-- >>> mkFxQuoteFail @Maybe @2 "EUR" "USD" (read "2021-12-31") (-1.16)
-- Nothing
mkFxQuoteFail
  :: MonadFail m
  => KnownNat s
  => Currency
  -- ^ Base currency (from) of the FX quotation.
  -> Currency
  -- ^ Quote currency (to) of the FX quotation.
  -> Day
  -- ^ Date of the FX quotation.
  -> Scientific
  -- ^ FX quotation rate, expected to be positive.
  -> m (FxQuote s)
mkFxQuoteFail ccy1 ccy2 date =
  either (fail . T.unpack) pure . mkFxQuoteError ccy1 ccy2 date


-- | Unsafe 'FxQuote' constructor that 'error's if it fails.
--
-- >>> :set -XTypeApplications
-- >>> mkFxQuoteUnsafe @2 "EUR" "USD" (read "2021-12-31") 1.16
-- MkFxQuote {fxQuotePair = EUR/USD, fxQuoteDate = 2021-12-31, fxQuoteRate = Refined 1.16}
-- >>> mkFxQuoteUnsafe @2 "EUR" "USD" (read "2021-12-31") (-1.16)
-- ...
-- ...Can not create FX Rate. Error was:   The predicate (GreaterThan 0) failed with the message: Value is not greater than 0
-- ...
mkFxQuoteUnsafe
  :: KnownNat s
  => Currency
  -- ^ Base currency (from) of the FX quotation.
  -> Currency
  -- ^ Quote currency (to) of the FX quotation.
  -> Day
  -- ^ Date of the FX quotation.
  -> Scientific
  -- ^ FX quotation rate, expected to be positive.
  -> FxQuote s
mkFxQuoteUnsafe ccy1 ccy2 date =
  either (error . T.unpack) id . mkFxQuoteError ccy1 ccy2 date


-- * FX Rate Quotation Database


-- $fxRateQuotationDatabase
--
-- >>> :set -XTypeApplications
-- >>> let database = addFxQuotes [mkFxQuoteUnsafe @8 "EUR" "USD" (read "2021-12-31") 1.13, mkFxQuoteUnsafe @8 "EUR" "TRY" (read "2021-12-31") 15.14] emptyFxQuoteDatabase
-- >>> findFxQuote database (CurrencyPair "EUR" "USD") (read "2021-12-31")
-- Just (MkFxQuote {fxQuotePair = EUR/USD, fxQuoteDate = 2021-12-31, fxQuoteRate = Refined 1.13000000})
-- >>> findFxQuote database (CurrencyPair "EUR" "TRY") (read "2021-12-31")
-- Just (MkFxQuote {fxQuotePair = EUR/TRY, fxQuoteDate = 2021-12-31, fxQuoteRate = Refined 15.14000000})
-- >>> findFxQuote database (CurrencyPair "EUR" "TRY") (read "2021-12-30")
-- Nothing
-- >>> findFxQuote database (CurrencyPair "EUR" "TRY") (read "2022-01-01")
-- Just (MkFxQuote {fxQuotePair = EUR/TRY, fxQuoteDate = 2021-12-31, fxQuoteRate = Refined 15.14000000})


-- | Type encoding for a dictionary-based FX rate quotation database for various
-- 'CurrencyPair' values.
type FxQuoteDatabase (n :: Nat) = SM.Map CurrencyPair (FxQuotePairDatabase n)


-- | Type encoding for FX rate quotation database for a 'CurrencyPair'.
data FxQuotePairDatabase (n :: Nat) = FxQuotePairDatabase
  { fxQuotePairDatabasePair :: !CurrencyPair
  , fxQuotePairDatabaseTable :: !(SM.Map Day (FxQuote n))
  , fxQuotePairDatabaseSince :: !Day
  , fxQuotePairDatabaseUntil :: !Day
  }
  deriving (Show)


-- | Attempts to find and return the FX quotation for a given 'CurrencyPair' as
-- of a give 'Day' in a given 'FxQuoteDatabase'.
findFxQuote
  :: KnownNat n
  => FxQuoteDatabase n
  -- ^ FX quotation database to perform the lookup on.
  -> CurrencyPair
  -- ^ Currency pair we are looking for the quotation for.
  -> Day
  -- ^ Date the quotation we look for is valid as of.
  -> Maybe (FxQuote n)
findFxQuote db pair date = SM.lookup pair db >>= findFxQuoteAux date


-- | Attempts to find and return the FX quotation as of a give 'Day' in a given
-- 'FxQuotePairDatabase'.
findFxQuoteAux :: KnownNat n => Day -> FxQuotePairDatabase n -> Maybe (FxQuote n)
findFxQuoteAux date db
  | date < fxQuotePairDatabaseSince db = Nothing
  | otherwise = case SM.lookup date (fxQuotePairDatabaseTable db) of
      Nothing -> findFxQuoteAux (addDays (-1) date) db
      Just fx -> Just fx


-- | Returns empty FX rate quotation database.
--
-- >>> :set -XTypeApplications
-- >>> emptyFxQuoteDatabase @8
-- fromList []
emptyFxQuoteDatabase
  :: KnownNat n
  => FxQuoteDatabase n
emptyFxQuoteDatabase = SM.empty


-- | Adds a list of FX rate quotations to the given database.
--
-- >>> :set -XTypeApplications
-- >>> let database = emptyFxQuoteDatabase @8
-- >>> addFxQuotes [] database
-- fromList []
-- >>> addFxQuotes [mkFxQuoteUnsafe @8 "EUR" "USD" (read "2021-01-31") 1.13] database
-- fromList [(EUR/USD,FxQuotePairDatabase {fxQuotePairDatabasePair = EUR/USD, fxQuotePairDatabaseTable = fromList [(2021-01-31,MkFxQuote {fxQuotePair = EUR/USD, fxQuoteDate = 2021-01-31, fxQuoteRate = Refined 1.13000000})], fxQuotePairDatabaseSince = 2021-01-31, fxQuotePairDatabaseUntil = 2021-01-31})]
-- >>> addFxQuotes [mkFxQuoteUnsafe @8 "EUR" "USD" (read "2021-01-31") 1.13, mkFxQuoteUnsafe @8 "USD" "EUR" (read "2021-01-31") 0.884956] database
-- fromList [(EUR/USD,FxQuotePairDatabase {fxQuotePairDatabasePair = EUR/USD, fxQuotePairDatabaseTable = fromList [(2021-01-31,MkFxQuote {fxQuotePair = EUR/USD, fxQuoteDate = 2021-01-31, fxQuoteRate = Refined 1.13000000})], fxQuotePairDatabaseSince = 2021-01-31, fxQuotePairDatabaseUntil = 2021-01-31}),(USD/EUR,FxQuotePairDatabase {fxQuotePairDatabasePair = USD/EUR, fxQuotePairDatabaseTable = fromList [(2021-01-31,MkFxQuote {fxQuotePair = USD/EUR, fxQuoteDate = 2021-01-31, fxQuoteRate = Refined 0.88495600})], fxQuotePairDatabaseSince = 2021-01-31, fxQuotePairDatabaseUntil = 2021-01-31})]
addFxQuotes
  :: KnownNat n
  => [FxQuote n]
  -> FxQuoteDatabase n
  -> FxQuoteDatabase n
addFxQuotes quotes database = foldl' (flip addFxQuote) database quotes


-- | Adds an FX rate quotation to the given database.
addFxQuote
  :: KnownNat n
  => FxQuote n
  -> FxQuoteDatabase n
  -> FxQuoteDatabase n
addFxQuote quote@(MkFxQuote pair _ _) database = case SM.lookup pair database of
  Nothing -> SM.insert pair (initFxQuotePairDatabase quote) database
  Just fpd -> SM.insert pair (updateFxQuotePairDatabase quote fpd) database


-- * Internal


-- | Initializes FX quote pair database with the given FX quote.
initFxQuotePairDatabase
  :: KnownNat n
  => FxQuote n
  -> FxQuotePairDatabase n
initFxQuotePairDatabase quote@(MkFxQuote pair date _) =
  FxQuotePairDatabase
    { fxQuotePairDatabasePair = pair
    , fxQuotePairDatabaseTable = SM.singleton date quote
    , fxQuotePairDatabaseSince = date
    , fxQuotePairDatabaseUntil = date
    }


-- | Updates an existing FX quote pair database with the given FX quote.
updateFxQuotePairDatabase
  :: KnownNat n
  => FxQuote n
  -> FxQuotePairDatabase n
  -> FxQuotePairDatabase n
updateFxQuotePairDatabase quote@(MkFxQuote _ date _) before =
  before
    { fxQuotePairDatabaseTable = SM.insert date quote (fxQuotePairDatabaseTable before)
    , fxQuotePairDatabaseSince = min (fxQuotePairDatabaseSince before) date
    , fxQuotePairDatabaseUntil = max (fxQuotePairDatabaseUntil before) date
    }
