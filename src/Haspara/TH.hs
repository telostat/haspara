-- | This module provides template-haskell functions for various "Haspara"
-- definitions.
module Haspara.TH where

import Data.Function (fix)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import GHC.TypeLits (KnownNat)
import Haspara.Currency (Currency, CurrencyPair (CurrencyPair), mkCurrencyError)
import Haspara.Quantity (Quantity, mkQuantityLossless)
import qualified Language.Haskell.TH.Syntax as TH


-- | Constructs a 'Quantity' value at compile-time using @-XTemplateHaskell@.
--
-- >>> $$(quantityTH 0.00) :: Quantity 2
-- 0.00
-- >>> $$(quantityTH 0.09) :: Quantity 2
-- 0.09
-- >>> $$(quantityTH 0.009) :: Quantity 2
-- ...
-- ..."Underflow while trying to create quantity: 9.0e-3"
-- ...
-- >>> $$(quantityTH 0.009) :: Quantity 3
-- 0.009
quantityTH :: KnownNat s => Scientific -> TH.Q (TH.TExp (Quantity s))
quantityTH = fix $ \loop -> fmap TH.TExp . either (fail . show) TH.lift . quantityWE (loop undefined)
  where
    -- This provides a work-around for the type-inference due the `s` type parameter.
    -- Trick is borrowed from the Haskell `refined` library.
    quantityWE :: KnownNat s => TH.Q (TH.TExp (Quantity s)) -> Scientific -> Either String (Quantity s)
    quantityWE = const mkQuantityLossless


-- | Constructs a 'Currency' value at compile-time using @-XTemplateHaskell@.
--
-- >>> $$(currencyTH "USD")
-- USD
-- >>> $$(currencyTH "usd")
-- ...
-- ...Currency code error! Expecting at least 3 uppercase ASCII letters, but received: usd
-- ...
currencyTH :: T.Text -> TH.Q (TH.TExp Currency)
currencyTH = either (fail . T.unpack) (fmap TH.TExp . TH.lift) . mkCurrencyError


-- | Constructs a 'CurrencyPair' value at compile-time using @-XTemplateHaskell@.
--
-- >>> $$(currencyPairTH "EUR" "USD")
-- EUR/USD
-- >>> $$(currencyPairTH "USD" "USD")
-- USD/USD
-- >>> $$(currencyPairTH "USD" "eur")
-- ...
-- ...Currency code error! Expecting at least 3 uppercase ASCII letters, but received: eur
-- ...
currencyPairTH :: T.Text -> T.Text -> TH.Q (TH.TExp CurrencyPair)
currencyPairTH = (either (fail . T.unpack) (fmap TH.TExp . TH.lift) .) . mkPair
  where
    mkPair :: T.Text -> T.Text -> Either T.Text CurrencyPair
    mkPair x y = CurrencyPair <$> mkCurrencyError x <*> mkCurrencyError y
