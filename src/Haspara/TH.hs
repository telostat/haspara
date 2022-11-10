-- | This module provides template-haskell functions for various "Haspara"
-- definitions.
module Haspara.TH where

import Data.Function (fix)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import GHC.TypeLits (KnownNat)
import Haspara.Currency (Currency, CurrencyPair (CurrencyPair), mkCurrencyError)
import Haspara.Quantity (Quantity, mkQuantityLossless)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH.Syntax


-- | Constructs a 'Quantity' value at compile-time using @-XTemplateHaskell@.
--
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedStrings
-- >>> :set -XTemplateHaskell
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
quantityTH :: KnownNat s => Scientific -> TH.Code TH.Q (Quantity s)
quantityTH = fix $ \loop -> either (TH.Syntax.liftCode . fail . show) TH.Syntax.liftTyped . quantityWE (loop undefined)
  where
    -- This provides a work-around for the type-inference due the `s` type parameter.
    -- Trick is borrowed from the Haskell `refined` library.
    quantityWE :: KnownNat s => TH.Code TH.Q (Quantity s) -> Scientific -> Either String (Quantity s)
    quantityWE = const mkQuantityLossless


-- | Constructs a 'Currency' value at compile-time using @-XTemplateHaskell@.
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTemplateHaskell
-- >>> $$(currencyTH "USD")
-- USD
-- >>> $$(currencyTH "usd")
-- ...
-- ...Currency code error! Expecting at least 3 uppercase ASCII letters, but received: usd
-- ...
currencyTH :: T.Text -> TH.Code TH.Q Currency
currencyTH = either (TH.Syntax.liftCode . fail . T.unpack) TH.Syntax.liftTyped . mkCurrencyError


-- | Constructs a 'CurrencyPair' value at compile-time using @-XTemplateHaskell@.
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XTemplateHaskell
-- >>> $$(currencyPairTH "EUR" "USD")
-- EUR/USD
-- >>> $$(currencyPairTH "USD" "USD")
-- USD/USD
-- >>> $$(currencyPairTH "USD" "eur")
-- ...
-- ... Currency code error! Expecting at least 3 uppercase ASCII letters, but received: eur
-- ...
currencyPairTH :: T.Text -> T.Text -> TH.Code TH.Q CurrencyPair
currencyPairTH cf ct = either (TH.Syntax.liftCode . fail . T.unpack) TH.Syntax.liftTyped (mkPair cf ct)
  where
    mkPair :: T.Text -> T.Text -> Either T.Text CurrencyPair
    mkPair x y = CurrencyPair <$> mkCurrencyError x <*> mkCurrencyError y
