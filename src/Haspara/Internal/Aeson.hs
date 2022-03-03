-- | This module provides helper definitions for "Data.Aeson".

module Haspara.Internal.Aeson where

import qualified Data.Char      as C
import qualified Deriving.Aeson as DA


-- | Type definition for string modifiers that uppercase a given symbol.
data UpperCase


instance DA.StringModifier UpperCase where
  getStringModifier = fmap C.toUpper
