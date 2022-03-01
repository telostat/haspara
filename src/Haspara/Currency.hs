-- | This module provides base data definitions and functions for 'Haspara'
-- library.
--
module Haspara.Currency
  ( -- * Currency
    -- &currency
    --
    -- ** Data Definition
    -- &currencyDataDefinition
    --
    Currency
  , currencyCode
    --
    -- ** Constructors
    -- &currencyConstructors
    --
  , currency
  , currencyFail
    --
    -- * Currency Pair
    -- &currencyPair
    --
    -- ** Data Definition
    -- &currencyPairDataDefinition
    --
  , CurrencyPair
  , toTuple
  , baseCurrency
  , quoteCurrency
    --
    -- ** Constructors
    -- &currencyPairConstructors
    --
  , currencyPair
  , currencyPairFail
  ) where

import Haspara.Internal.Currency
       ( Currency(currencyCode)
       , CurrencyPair
       , baseCurrency
       , currency
       , currencyFail
       , currencyPair
       , currencyPairFail
       , quoteCurrency
       , toTuple
       )
