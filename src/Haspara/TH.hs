-- | This module provides template-haskell functions for various 'Haspara.Core.Base'
-- definitions.
--

module Haspara.TH where

import           Control.Monad              (join)
import           Data.Function              (fix)
import           Data.Scientific            (Scientific)
import qualified Data.Text                  as T
import           GHC.TypeLits               (KnownNat)
import           Haspara.Currency           (Currency, CurrencyPair, currency, currencyPair)
import           Haspara.Quantity           (Quantity, quantityLossless)
import qualified Language.Haskell.TH.Syntax as TH


-- | Constructs a 'Quantity' value at compile-time using @-XTemplateHaskell@.
--
-- >>> :set -XDataKinds
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
    quantityWE = const quantityLossless


-- | Constructs a 'Currency' value at compile-time using @-XTemplateHaskell@.
--
-- >>> $$(currencyTH "USD")
-- USD
-- >>> $$(currencyTH "usd")
-- ...
-- ...Currency code error! Expecting at least 3 uppercase characters, but received: "usd"
-- ...
currencyTH :: T.Text -> TH.Q (TH.TExp Currency)
currencyTH = either fail (fmap TH.TExp . TH.lift) . currency


-- | Constructs a 'CurrencyPair' value at compile-time using @-XTemplateHaskell@.
--
-- >>> $$(currencyPairTH "EUR" "USD")
-- EUR/USD
-- >>> $$(currencyPairTH "USD" "USD")
-- ...
-- ...Can not create currency pair from same currencies: USD and USD
-- ...
-- >>> $$(currencyPairTH "USD" "eur")
-- ...
-- ...Currency code error! Expecting at least 3 uppercase characters, but received: "eur"
-- ...
currencyPairTH :: T.Text -> T.Text -> TH.Q (TH.TExp CurrencyPair)
currencyPairTH = (either fail (fmap TH.TExp . TH.lift) .) . mkPair
  where
    mkPair :: T.Text -> T.Text -> Either String CurrencyPair
    mkPair x y = join $ currencyPair <$> currency x <*> currency y


-- -- | Constructs an 'FXQuote' value at compile-time using @-XTemplateHaskell@.
-- --
-- -- >>> :set -XDataKinds
-- -- >>> $$(fxquoteTH (read "2021-01-01") "EUR" "USD" 10) :: FXQuote 2
-- -- ("EUR/USD","2021-01-01","10.00")
-- fxquoteTH :: KnownNat s => Date -> T.Text -> T.Text -> Scientific -> TH.Q (TH.TExp (FXQuote s))
-- fxquoteTH d c1 c2 = fix $ \loop -> fmap TH.TExp . either (fail . show) TH.lift . fxquoteWE (loop undefined)
--   where
--     -- This provides a work-around for the type-inference due the `s` type parameter.
--     -- Trick is borrowed from the Haskell `refined` library.
--     fxquoteWE :: KnownNat s => TH.Q (TH.TExp (FXQuote s)) -> Scientific -> Either String (FXQuote s)
--     fxquoteWE _ v = do
--       xc1 <- currency c1
--       xc2 <- currency c2
--       either (Left . show) Right $ fxquote d xc1 xc2 v
