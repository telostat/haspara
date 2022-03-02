-- | This module provides definitions for modeling and working with FX rates.

{-# LANGUAGE DataKinds #-}

module Haspara.FXQuote where

import           Control.Monad.Except (MonadError(throwError), join)
import           Data.Aeson           ((.:), (.=))
import qualified Data.Aeson           as Aeson
import qualified Data.Map.Strict      as SM
import           Data.Scientific      (Scientific)
import           Data.Time            (Day, addDays)
import           GHC.TypeLits         (KnownNat, Nat)
import           Haspara.Currency     (Currency, CurrencyPair(CurrencyPair, currencyPairBase, currencyPairQuote))
import           Haspara.Quantity     (Quantity(..), quantity)
import           Numeric.Decimal      (toScientificDecimal)
import           Refined              (Positive, Refined, refineError, unrefine)

-- * FX Rate Data Definition
-- &fXQuoteValue


-- | Type encoding for FX rates.
data FXQuote (s :: Nat) = MkFXQuote
  { -- | Actual date of the FX rate.
    fxQuoteDate :: !Day
    -- | Currency pair of the FX rate.
  , fxQuotePair :: !CurrencyPair
    -- | Rate value of the FX rate.
  , fxQuoteRate :: !(Refined Positive (Quantity s))
  } deriving (Eq, Ord)


instance (KnownNat s) => Show (FXQuote s) where
  show (MkFXQuote d p r) = show (show p, show d, show (unrefine r))


-- | 'Aeson.FromJSON' instance for 'Currency'
--
-- >>> :set -XDataKinds
-- >>> Aeson.eitherDecode "{\"date\": \"2021-01-01\", \"ccy1\": \"USD\", \"ccy2\": \"SGD\", \"rate\": 1.35}" :: Either String (FXQuote 2)
-- Right ("USD/SGD","2021-01-01","1.35")
-- >>> Aeson.eitherDecode "{\"date\": \"2021-01-01\", \"ccy1\": \"USD\", \"ccy2\": \"SGD\", \"rate\": 1.354}" :: Either String (FXQuote 2)
-- Right ("USD/SGD","2021-01-01","1.35")
-- >>> Aeson.eitherDecode "{\"date\": \"2021-01-01\", \"ccy1\": \"USD\", \"ccy2\": \"SGD\", \"rate\": 1.355}" :: Either String (FXQuote 2)
-- Right ("USD/SGD","2021-01-01","1.36")
-- >>> Aeson.eitherDecode "{\"date\": \"2021-01-01\", \"ccy1\": \"USD\", \"ccy2\": \"SGD\", \"rate\": 1.356}" :: Either String (FXQuote 2)
-- Right ("USD/SGD","2021-01-01","1.36")
-- >>> Aeson.eitherDecode "{\"date\": \"2021-01-01\", \"ccy1\": \"USD\", \"ccy2\": \"SGD\", \"rate\": 1.364}" :: Either String (FXQuote 2)
-- Right ("USD/SGD","2021-01-01","1.36")
-- >>> Aeson.eitherDecode "{\"date\": \"2021-01-01\", \"ccy1\": \"USD\", \"ccy2\": \"SGD\", \"rate\": 1.365}" :: Either String (FXQuote 2)
-- Right ("USD/SGD","2021-01-01","1.36")
-- >>> Aeson.eitherDecode "{\"date\": \"2021-01-01\", \"ccy1\": \"USD\", \"ccy2\": \"SGD\", \"rate\": 1.366}" :: Either String (FXQuote 2)
-- Right ("USD/SGD","2021-01-01","1.37")
-- >>> Aeson.eitherDecode "{\"date\": \"2021-01-01\", \"ccy1\": \"USD\", \"ccy2\": \"USD\", \"rate\": 1}" :: Either String (FXQuote 2)
-- Right ("USD/USD","2021-01-01","1.00")
-- >>> Aeson.eitherDecode "{\"date\": \"2021-01-01\", \"ccy1\": \"USD\", \"ccy2\": \"SGD\", \"rate\": -1.35}" :: Either String (FXQuote 2)
-- Left "Error in $: Can not create FX Rate. Error was:   The predicate (GreaterThan 0) failed with the message: Value is not greater than 0\n"
instance (KnownNat s) => Aeson.FromJSON (FXQuote s) where
  parseJSON = Aeson.withObject "FXQuote" $ \o -> join $ fxquoteFail
    <$> o .: "date"
    <*> o .: "ccy1"
    <*> o .: "ccy2"
    <*> o .: "rate"


-- | 'Aeson.ToJSON' instance for 'Currency'
--
-- >>> :set -XDataKinds
-- >>> let rate = fxquoteUnsafe (read "2021-01-01") "USD" "SGD" 1.35 :: FXQuote 2
-- >>> Aeson.encode rate
-- "{\"rate\":1.35,\"ccy2\":\"SGD\",\"date\":\"2021-01-01\",\"ccy1\":\"USD\"}"
instance (KnownNat s) => Aeson.ToJSON (FXQuote s) where
  toJSON (MkFXQuote d cp v) = Aeson.object
    [ "date" .= d
    , "ccy1" .= currencyPairBase cp
    , "ccy2" .= currencyPairQuote cp
    , "rate" .= (toScientificDecimal . unQuantity . unrefine) v
    ]


-- * Constructors
-- &constructors


-- | Smart constructor for 'FXQuote' values within 'MonadError' context.
fxquote
  :: (KnownNat s, MonadError String m)
  => Day         -- ^ Date of the FX rate.
  -> Currency    -- ^ First currency (from) of the FX rate.
  -> Currency    -- ^ Second currency (to) of the FX rate.
  -> Scientific  -- ^ FX rate value.
  -> m (FXQuote s)
fxquote d c1 c2 v = either (throwError . (<>) "Can not create FX Rate. Error was: ") pure $ do
  let pair = CurrencyPair c1 c2
  pval <- either (Left . show) pure $ refineError (quantity v)
  pure $ MkFXQuote d pair pval


-- | Smart constructor for 'FXQuote' values within 'MonadFail' context.
fxquoteFail
  :: (KnownNat s, MonadFail m)
  => Day         -- ^ Date of the FX rate.
  -> Currency    -- ^ First currency (from) of the FX rate.
  -> Currency    -- ^ Second currency (to) of the FX rate.
  -> Scientific  -- ^ FX rate value.
  -> m (FXQuote s)
fxquoteFail d c1 c2 = either fail pure . fxquote d c1 c2


-- | Unsafe 'FXQuote' constructor that 'error's if it fails.
fxquoteUnsafe
  :: KnownNat s
  => Day         -- ^ Date of the FX rate.
  -> Currency    -- ^ First currency (from) of the FX rate.
  -> Currency    -- ^ Second currency (to) of the FX rate.
  -> Scientific  -- ^ FX rate value.
  -> FXQuote s
fxquoteUnsafe d c1 c2 = either error id . fxquote d c1 c2


type FXQuoteDatabase (n :: Nat) = SM.Map CurrencyPair (FXQuotePairDatabase n)


data FXQuotePairDatabase (n :: Nat) = FXQuotePairDatabase
  { fxQuotePairDatabasePair  :: !CurrencyPair
  , fxQuotePairDatabaseTable :: !(SM.Map Day (FXQuote n))
  , fxQuotePairDatabaseSince :: !Day
  , fxQuotePairDatabaseUntil :: !Day
  }


findFXQuote :: KnownNat n => FXQuoteDatabase n -> CurrencyPair -> Day -> Maybe (FXQuote n)
findFXQuote db cp d = case SM.lookup cp db of
  Nothing  -> Nothing
  Just pdb -> findFXQuoteAux d pdb


findFXQuoteAux :: KnownNat n => Day -> FXQuotePairDatabase n -> Maybe (FXQuote n)
findFXQuoteAux d db
  | d < fxQuotePairDatabaseSince db = Nothing
  | otherwise = case SM.lookup d (fxQuotePairDatabaseTable db) of
      Nothing -> findFXQuoteAux (addDays (-1) d) db
      Just fx -> Just fx
