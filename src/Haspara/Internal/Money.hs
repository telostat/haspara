{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Haspara.Internal.Money where

import           Control.Applicative       ((<|>))
import           Data.Aeson                ((.:), (.=))
import qualified Data.Aeson                as Aeson
import           Data.Scientific           (Scientific)
import           GHC.TypeLits              (KnownNat, Nat)
import           Haspara.Internal.Currency (Currency, baseCurrency, quoteCurrency)
import           Haspara.Internal.Date     (Date)
import           Haspara.Internal.FXQuote  (FXQuote(fxQuotePair, fxQuoteRate))
import           Haspara.Internal.Quantity (Quantity, quantity, times)
import           Refined                   (unrefine)


data Money (s :: Nat) =
    MoneySome Date Currency (Quantity s)
  | MoneyZero
  | MoneyFail String
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'Money'.
--
-- >>> Aeson.decode "{\"qty\":42,\"ccy\":\"USD\",\"date\":\"2021-01-01\"}" :: Maybe (Money 2)
-- Just (MoneySome 2021-01-01 USD 42.00)
-- >>> Aeson.decode "0" :: Maybe (Money 2)
-- Just MoneyZero
-- >>> Aeson.decode "{\"error\": \"oops\"}" :: Maybe (Money 2)
-- Just (MoneyFail "oops")
instance (KnownNat s) => Aeson.FromJSON (Money s) where
  parseJSON (Aeson.Number 0) = pure MoneyZero
  parseJSON (Aeson.Object obj) = parseSome obj <|> parseFail obj
    where
      parseSome o = MoneySome
        <$> o .: "date"
        <*> o .: "ccy"
        <*> o .: "qty"
      parseFail o = MoneyFail <$> o .: "error"
  parseJSON x = fail ("Not a monetary value: " <> show x)



-- | 'Aeson.ToJSON' instance for 'Money'.
--
-- >>> Aeson.encode (MoneySome (read "2021-01-01") ("USD" :: Currency) (42 :: Quantity 0))
-- "{\"qty\":42,\"ccy\":\"USD\",\"date\":\"2021-01-01\"}"
-- >>> Aeson.encode (MoneyZero :: Money 2)
-- "0"
-- >>> Aeson.encode (MoneyFail "oops" :: Money 2)
-- "{\"error\":\"oops\"}"
instance (KnownNat s) => Aeson.ToJSON (Money s) where
  toJSON (MoneySome d c q) = Aeson.object [ "date" .= d, "ccy" .= c, "qty" .= q ]
  toJSON MoneyZero         = Aeson.Number 0
  toJSON (MoneyFail s)     = Aeson.object ["error" .= s]


mkMoney :: KnownNat s => Date -> Currency -> Quantity s -> Money s
mkMoney = MoneySome


mkMoneyFromScientific :: KnownNat s => Date -> Currency -> Scientific -> Money s
mkMoneyFromScientific d c s = mkMoney d c (quantity s)


moneyDate :: KnownNat s => Money s -> Maybe Date
moneyDate (MoneySome d _ _) = Just d
moneyDate MoneyZero         = Nothing
moneyDate (MoneyFail _)     = Nothing


moneyCurrency :: KnownNat s => Money s -> Maybe Currency
moneyCurrency (MoneySome _ c _) = Just c
moneyCurrency MoneyZero         = Nothing
moneyCurrency (MoneyFail _)     = Nothing


moneyQuantity :: KnownNat s => Money s -> Maybe (Quantity s)
moneyQuantity (MoneySome _ _ q) = Just q
moneyQuantity MoneyZero         = Nothing
moneyQuantity (MoneyFail _)     = Nothing


-- | Converts the given 'Money' value to another given currency with the given
-- rate.
--
-- >>> import Haspara
-- >>> let eur = either error id $ currency "EUR"
-- >>> let usd = either error id $ currency "USD"
-- >>> let date = read "2021-01-01" :: Date
-- >>> let eurmoney = mkMoney date eur (quantity 0.42 :: Quantity 2) :: Money 2
-- >>> convert eurmoney eur (quantity 1 :: Quantity 4)
-- MoneySome 2021-01-01 EUR 0.42
-- >>> convert eurmoney usd (quantity 1 :: Quantity 4)
-- MoneySome 2021-01-01 USD 0.42
-- >>> convert eurmoney usd (quantity 1.1516 :: Quantity 4)
-- MoneySome 2021-01-01 USD 0.48
convert :: (KnownNat s, KnownNat k) => Money s -> Currency -> Quantity k -> Money s
convert MoneyZero _ _                    = MoneyZero
convert x@(MoneyFail _) _ _              = x
convert x@(MoneySome d cbase q) cquot rate
  | cbase == cquot && rate == 1 = x
  | cbase == cquot && rate /= 1 = MoneyFail $ "Attempting to convert from same currency with rate != 1: " <> show x <> " to " <> show cquot <> " with " <> show rate
  | otherwise                   = MoneySome d cquot (times q rate)


-- | Converts the given 'Money' value to another currency with the given
-- 'FXQuote'.
convertWithQuote :: (KnownNat s, KnownNat k) => Money s -> FXQuote k -> Money s
convertWithQuote MoneyZero _                    = MoneyZero
convertWithQuote x@(MoneyFail _) _              = x
convertWithQuote x@(MoneySome _ cbase _) quote
  | cbase /= baseCurrency (fxQuotePair quote) = MoneyFail $ "Attempting to convert with incompatible base currency: " <> show x <> " with " <> show quote
  | otherwise = convert x (quoteCurrency (fxQuotePair quote)) (unrefine $ fxQuoteRate quote)
