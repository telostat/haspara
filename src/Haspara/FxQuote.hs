-- | This module provides definitions for modeling and working with foreign
-- exchange (FX) rate quotations.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Haspara.FxQuote where

import           Control.Monad.Except (MonadError(throwError))
import qualified Data.Map.Strict      as SM
import           Data.Scientific      (Scientific)
import qualified Data.Text            as T
import           Data.Time            (Day, addDays)
import qualified Deriving.Aeson.Stock as DAS
import           GHC.TypeLits         (KnownNat, Nat)
import           Haspara.Currency     (Currency, CurrencyPair(CurrencyPair))
import           Haspara.Quantity     (Quantity(..), mkQuantity)
import           Refined              (Positive, Refined, refineError)


-- * FX Rate Quotation
-- $fxRateQuotation


-- | Type encoding for FX rate quotations with fixed precision.
--
-- An FX rate quotation is a 3-tuple of:
--
-- 1. a currency pair the rate is quoted for, and
-- 2. a date that the quotation is effective as of,
-- 3. a (positive) rate as the value of the quotation.
--
-- >>>
data FxQuote (s :: Nat) = MkFxQuote
  { fxQuotePair :: !CurrencyPair  -- ^ Currency pair of the FX rate.
  , fxQuoteDate :: !Day  -- ^ Actual date of the FX rate.
  , fxQuoteRate :: !(Refined Positive (Quantity s))  -- ^ (Positive) rate value of the FX rate.
  }
  deriving (Eq, DAS.Generic, Ord, Show)
  deriving (DAS.FromJSON, DAS.ToJSON) via DAS.PrefixedSnake "fxQuote" (FxQuote s)


-- | Smart constructor for 'FxQuote' values within @'MonadError' 'T.Text'@
-- context.
--
-- The rate is expected to be a positive value. If it is not, the function will
-- throw an error.
--
-- >>> mkFxQuoteError @(Either _) @2 (read "2021-12-31") "EUR" "USD" 1.16
-- Right (MkFxQuote {fxQuotePair = EUR/USD, fxQuoteDate = 2021-12-31, fxQuoteRate = Refined 1.16})
-- >>> mkFxQuoteError @(Either _) @2 (read "2021-12-31") "EUR" "USD" (-1.16)
-- Left "Can not create FX Rate. Error was:   The predicate (GreaterThan 0) failed with the message: Value is not greater than 0\n"
mkFxQuoteError
  :: MonadError T.Text m
  => KnownNat s
  => Day         -- ^ Date of the FX quotation.
  -> Currency    -- ^ Base currency (from) of the FX quotation.
  -> Currency    -- ^ Quote currency (to) of the FX quotation.
  -> Scientific  -- ^ FX quotation rate, expected to be positive.
  -> m (FxQuote s)
mkFxQuoteError date ccy1 ccy2 rate =
  either (throwError . (<>) "Can not create FX Rate. Error was: ") pure $ do
    pval <- either (Left . T.pack . show) pure $ refineError (mkQuantity rate)
    pure $ MkFxQuote (CurrencyPair ccy1 ccy2) date pval


-- | Smart constructor for 'FxQuote' values within 'MonadFail' context.
--
-- The rate is expected to be a positive value. If it is not, the function will
-- fail.
-- >>> mkFxQuoteFail @Maybe @2 (read "2021-12-31") "EUR" "USD" 1.16
-- Just (MkFxQuote {fxQuotePair = EUR/USD, fxQuoteDate = 2021-12-31, fxQuoteRate = Refined 1.16})
-- >>> mkFxQuoteFail @Maybe @2 (read "2021-12-31") "EUR" "USD" (-1.16)
-- Nothing
mkFxQuoteFail
  :: MonadFail m
  => KnownNat s
  => Day         -- ^ Date of the FX quotation.
  -> Currency    -- ^ Base currency (from) of the FX quotation.
  -> Currency    -- ^ Quote currency (to) of the FX quotation.
  -> Scientific  -- ^ FX quotation rate, expected to be positive.
  -> m (FxQuote s)
mkFxQuoteFail date ccy1 ccy2 =
  either (fail . T.unpack) pure . mkFxQuoteError date ccy1 ccy2


-- | Unsafe 'FxQuote' constructor that 'error's if it fails.
--
-- >>> mkFxQuoteUnsafe @2 (read "2021-12-31") "EUR" "USD" 1.16
-- MkFxQuote {fxQuotePair = EUR/USD, fxQuoteDate = 2021-12-31, fxQuoteRate = Refined 1.16}
-- >>> mkFxQuoteUnsafe @2 (read "2021-12-31") "EUR" "USD" (-1.16)
-- ...
-- ...Can not create FX Rate. Error was:   The predicate (GreaterThan 0) failed with the message: Value is not greater than 0
-- ...
mkFxQuoteUnsafe
  :: KnownNat s
  => Day         -- ^ Date of the FX quotation.
  -> Currency    -- ^ Base currency (from) of the FX quotation.
  -> Currency    -- ^ Quote currency (to) of the FX quotation.
  -> Scientific  -- ^ FX quotation rate, expected to be positive.
  -> FxQuote s
mkFxQuoteUnsafe date ccy1 ccy2 =
  either (error . T.unpack) id . mkFxQuoteError date ccy1 ccy2


-- * FX Rate Quotation Database
-- $fxRateQuotationDatabase


-- | Type encoding for a dictionary-based FX rate quotation database for various
-- 'CurrencyPair' values.
type FxQuoteDatabase (n :: Nat) = SM.Map CurrencyPair (FxQuotePairDatabase n)


-- | Type encoding for FX rate quotation database for a 'CurrencyPair'.
data FxQuotePairDatabase (n :: Nat) = FxQuotePairDatabase
  { fxQuotePairDatabasePair  :: !CurrencyPair
  , fxQuotePairDatabaseTable :: !(SM.Map Day (FxQuote n))
  , fxQuotePairDatabaseSince :: !Day
  , fxQuotePairDatabaseUntil :: !Day
  }


-- | Attempts to find and return the FX quotation for a given 'CurrencyPair' as
-- of a give 'Day' in a given 'FxQuoteDatabase'.
findFxQuote
  :: KnownNat n
  => FxQuoteDatabase n  -- ^ FX quotation database to perform the lookup on.
  -> CurrencyPair       -- ^ Currency pair we are looking for the quotation for.
  -> Day                -- ^ Date the quotation we look for is valid as of.
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
