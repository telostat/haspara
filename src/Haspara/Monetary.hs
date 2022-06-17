-- | This module provides definitions for modeling and working with monetary
-- values.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Haspara.Monetary where

import           Control.Exception      (Exception)
import           Control.Monad          (when)
import           Control.Monad.Catch    (MonadThrow(throwM))
import qualified Data.Aeson             as Aeson
import           Data.Time              (Day)
import           GHC.Generics           (Generic)
import           GHC.Stack              (HasCallStack)
import           GHC.TypeLits           (KnownNat, Nat)
import           Haspara.Currency       (Currency, CurrencyPair(..))
import           Haspara.FxQuote        (FxQuote(..))
import           Haspara.Internal.Aeson (commonAesonOptions)
import           Haspara.Quantity       (Quantity, times)
import           Refined                (unrefine)


-- | Type encoding for dated monetary values.
--
-- A dated monetary value is a 3-tuple of:
--
-- 1. a date when the monetary value is effective as of,
-- 2. the currency of the monetary value, and
-- 3. the quantity of the monetary value.
data Money (s :: Nat) = Money
  { moneyDate     :: !Day
  , moneyCurrency :: !Currency
  , moneyQuantity :: !(Quantity s)
  }
  deriving (Eq, Generic, Ord, Show)


instance KnownNat s => Aeson.FromJSON (Money s) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "money"


instance KnownNat s => Aeson.ToJSON (Money s) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "money"


-- | Type encoding of a monetary context.
class MonadThrow m => Monetary m where
  -- | Converts the given monetary value in one currency to another currency.
  --
  -- Note that the conversion is performed with an FX rate quotation as of the
  -- date of the given monetary value.
  convertM
    :: HasCallStack
    => KnownNat s
    => Currency
    -> Money s
    -> m (Money s)

  -- | Converts the given monetary value in one currency to another currency as
  -- of the given date.
  --
  -- The rule is:
  --
  -- @
  -- convertAsofM <DATE2> <CCY2> (Money <DATE1> <CCY1> <QTY1>) === convertM <CCY2> (Money <DATE2> <CCY1> <QTY1>)
  -- @
  convertAsofM
    :: HasCallStack
    => KnownNat s
    => Day
    -> Currency
    -> Money s
    -> m (Money s)
  convertAsofM date ccyN (Money _ ccy qty) = convertM ccyN (Money date ccy qty)


-- | Attempts to convert the given 'Money' to another using the given 'FxQuote'
-- value.
--
-- This function runs some guards before attempting to do the conversion:
--
-- 1. Base currency of the FX rate quotation should be the same as the currency
--    of the monetary value, throws 'IncompatibleCurrenciesException' otherwise.
-- 2. Date of the FX rate quotation should be equal to or greater than the date
--    of the monetary value, throws 'IncompatibleDatesException' otherwise.
-- 3. Rate of the FX rate quotation should be @1@ if the base and quote
--    quotation are same, throws 'InconsistentFxQuoteException' otherwise.
convert
  :: HasCallStack
  => MonadThrow m
  => KnownNat s
  => KnownNat k
  => Money s
  -> FxQuote k
  -> m (Money s)
convert (Money date ccy qty) quote@(MkFxQuote (CurrencyPair ccy1 ccy2) asof rate) = do
  when (ccy /= ccy1) (throwM (IncompatibleCurrenciesException ccy ccy1))
  when (asof < date) (throwM (IncompatibleDatesException date asof))
  when (ccy1 == ccy2 && unrefine rate /= 1) (throwM (InconsistentFxQuoteException quote))
  pure (Money asof ccy2 (times qty (unrefine rate)))


-- | Type encoding of exceptions thrown by the `Haspara.Monetary` module.
data MonetaryException where
  -- | Indicates that we received a currency other than the expected currency.
  IncompatibleCurrenciesException
    :: HasCallStack
    => Currency  -- ^ Expected currency
    -> Currency  -- ^ Received currency
    -> MonetaryException

  -- | Indicates that we received a currency other than the expected currency.
  IncompatibleDatesException
    :: HasCallStack
    => Day  -- ^ Date on and onwards of interest
    -> Day  -- ^ Date received
    -> MonetaryException

  -- | Indicates that we received a currency other than the expected currency.
  InconsistentFxQuoteException
    :: forall (s :: Nat). (HasCallStack, KnownNat s)
    => FxQuote s  -- ^ FX rate quotation that is interpreted as inconsistent.
    -> MonetaryException


deriving instance Show MonetaryException


instance Exception MonetaryException
