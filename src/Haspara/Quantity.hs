-- | This module provides definitions and functions to encode and work on
-- quantities with fixed decimal points.
--
module Haspara.Quantity
  ( -- * Data Definition
    -- &dataDefinition
    --
    Quantity
  , unQuantity
    --
    -- ** Constructors
    -- &constructors
    --
  , quantity
  , quantityLossless
    --
    -- ** Operations
    -- &operations
    --
  , roundQuantity
  , times
  , timesLossless
  ) where

import Haspara.Internal.Quantity (Quantity(unQuantity), quantity, quantityLossless, roundQuantity, times, timesLossless)
