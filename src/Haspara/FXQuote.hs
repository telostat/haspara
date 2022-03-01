-- | This module provides base data definitions and functions for 'Haspara'
-- library.
--
module Haspara.FXQuote
  ( --
    -- * FX Quote
    -- &fxquote
    --
    -- ** Data Definition
    -- &fxquoteDataDefinition
    --
    FXQuote
  , fxQuoteDate
  , fxQuotePair
  , fxQuoteRate
    --
    -- ** Constructors
    -- &fxquoteConstructors
    --
  , fxquote
  , fxquoteFail
    --
    -- * FX Quote Database
    -- *fxquoteDatabase
    --
  , FXQuoteDatabase
  , FXQuotePairDatabase(..)
  , findFXQuote
  ) where

import Haspara.Internal.FXQuote         (FXQuote(fxQuoteDate, fxQuotePair, fxQuoteRate), fxquote, fxquoteFail)
import Haspara.Internal.FXQuoteDatabase (FXQuoteDatabase, FXQuotePairDatabase(..), findFXQuote)
