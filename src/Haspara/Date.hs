-- | This module provides definitions and functions to encode and work on date
-- values.
--
module Haspara.Date
  ( -- * Date
    -- &date
    --
    -- ** Definition
    -- &definition
    --
    Date
    --
    --
    -- ** Constructors
    -- &constructors
    --
  , fromDay
  , fromYMD
  , fromString
  , fromFormattedString
  , fromText
  , fromFormattedText
    --
    -- ** Conversions
    -- &conversions
    --
  , toDay
  , toYMD
  , toString
  , toFormattedString
  , toText
  , toFormattedText
    --
    -- ** Operations
    -- &operations
    --
  , addDays
  ) where

import Haspara.Internal.Date
       ( Date
       , addDays
       , fromDay
       , fromFormattedString
       , fromFormattedText
       , fromString
       , fromText
       , fromYMD
       , toDay
       , toFormattedString
       , toFormattedText
       , toString
       , toText
       , toYMD
       )
