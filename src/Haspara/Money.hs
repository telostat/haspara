-- | This module provides base data definitions and functions for 'Haspara'
-- library.
--
module Haspara.Money
  ( --
    -- * Money
    -- &money
    --
    -- ** Data Definition
    -- &quantityDataDefinition
    --
    Money(..)
  , moneyDate
  , moneyCurrency
  , moneyQuantity
    --
    -- ** Constructors
    -- &fxquoteConstructors
    --
  , mkMoney
  , mkMoneyFromScientific
    --
    -- ** Operations
    -- &operations
  , convert
  , convertWithQuote
  ) where

import Haspara.Internal.Money
       ( Money(..)
       , convert
       , convertWithQuote
       , mkMoney
       , mkMoneyFromScientific
       , moneyCurrency
       , moneyDate
       , moneyQuantity
       )
