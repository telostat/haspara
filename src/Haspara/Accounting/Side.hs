{-# LANGUAGE OverloadedStrings #-}

-- | This module provides definitions for and functions to work with
-- Debit/Credit dichotomy which is essential to double-entry bookkeeping.
--
-- In our concept, we refer to this dichotomy as "Side" (materialized via 'Side'
-- sum-type) which is either "Debit" (materialized via 'SideDebit' nullary data
-- constructor) or "Dredit" (materialized via 'SideCredit' nullary data
-- constructor).
--
-- This module provides 'Aeson.FromJSON' and 'Aeson.ToJSON' instances for 'Side'
-- as well. Following accounting conventions, we chose the JSON value for
-- "Debit" as @"db"@, and for "Credit" as @"cr"@.
module Haspara.Accounting.Side where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.TypeLits (KnownNat)
import Haspara.Accounting.Account (AccountKind (..))
import Haspara.Quantity (Quantity)


-- | Data definition for encoding the debit/credit indicator.
data Side = SideDebit | SideCredit
  deriving (Eq, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'Side'.
--
-- >>> Aeson.eitherDecode "\"db\"" :: Either String Side
-- Right SideDebit
-- >>> Aeson.eitherDecode "\"cr\"" :: Either String Side
-- Right SideCredit
-- >>> Aeson.eitherDecode "\"hebele\"" :: Either String Side
-- Left "Error in $: Unkown side indicator: \"hebele\". Expecting one of \"db\" or \"cr\""
instance Aeson.FromJSON Side where
  parseJSON = Aeson.withText "Side" $ \t -> case t of
    "db" -> pure SideDebit
    "cr" -> pure SideCredit
    _ -> fail $ "Unkown side indicator: \"" <> T.unpack t <> "\". Expecting one of \"db\" or \"cr\""


-- | 'Aeson.ToJSON' instance for 'Side'.
--
-- >>> Aeson.encode SideDebit
-- "\"db\""
-- >>> Aeson.encode SideCredit
-- "\"cr\""
-- >>> Aeson.decode (Aeson.encode SideDebit) == Just SideDebit
-- True
-- >>> Aeson.decode (Aeson.encode SideCredit) == Just SideCredit
-- True
instance Aeson.ToJSON Side where
  toJSON SideDebit = Aeson.String "db"
  toJSON SideCredit = Aeson.String "cr"


-- | Gives the other side.
--
-- >>> otherSide SideDebit
-- SideCredit
-- >>> otherSide SideCredit
-- SideDebit
otherSide :: Side -> Side
otherSide SideDebit = SideCredit
otherSide SideCredit = SideDebit


-- | Computes the 'Side' by the given 'AccountKind' and the sign of the given
-- 'Quantity'.
--
-- The sign of the 'Quantity' is indeed a proxy for whether the event of the
-- 'Quantity' is an increment (@+1@) or decrement (@-1@) event.
--
-- @0@ quantities are considered to originate from an increment event. So far,
-- this seems to be a safe assumption that gives us totality in the context of
-- this function.
--
-- Note the following mapping as a guide:
--
-- +-----------------------+----------+----------+
-- | Kind of account       | Debit    | Credit   |
-- +-----------------------+----------+----------+
-- | Asset                 | Increase | Decrease |
-- +-----------------------+----------+----------+
-- | Liability             | Decrease | Increase |
-- +-----------------------+----------+----------+
-- | Equity/Capital        | Decrease | Increase |
-- +-----------------------+----------+----------+
-- | Income/Revenue        | Decrease | Increase |
-- +-----------------------+----------+----------+
-- | Expense/Cost/Dividend | Increase | Decrease |
-- +-----------------------+----------+----------+
--
-- >>> :set -XDataKinds
-- >>> import Haspara.Quantity
-- >>> let decrement = mkQuantity (-0.42) :: Quantity 2
-- >>> let nocrement = mkQuantity 0 :: Quantity 2
-- >>> let increment = mkQuantity 0.42 :: Quantity 2
-- >>> fmap (sideByAccountKind AccountKindAsset) [decrement, nocrement, increment]
-- [SideCredit,SideDebit,SideDebit]
-- >>> fmap (sideByAccountKind AccountKindLiability) [decrement, nocrement, increment]
-- [SideDebit,SideCredit,SideCredit]
-- >>> fmap (sideByAccountKind AccountKindEquity) [decrement, nocrement, increment]
-- [SideDebit,SideCredit,SideCredit]
-- >>> fmap (sideByAccountKind AccountKindRevenue) [decrement, nocrement, increment]
-- [SideDebit,SideCredit,SideCredit]
-- >>> fmap (sideByAccountKind AccountKindExpense) [decrement, nocrement, increment]
-- [SideCredit,SideDebit,SideDebit]
sideByAccountKind
  :: KnownNat precision
  => AccountKind
  -> Quantity precision
  -> Side
sideByAccountKind k q = case (k, signum q >= 0) of
  (AccountKindAsset, False) -> SideCredit
  (AccountKindAsset, True) -> SideDebit
  (AccountKindLiability, False) -> SideDebit
  (AccountKindLiability, True) -> SideCredit
  (AccountKindEquity, False) -> SideDebit
  (AccountKindEquity, True) -> SideCredit
  (AccountKindRevenue, False) -> SideDebit
  (AccountKindRevenue, True) -> SideCredit
  (AccountKindExpense, False) -> SideCredit
  (AccountKindExpense, True) -> SideDebit


-- | Returns the "normal" side for a given 'AccountKind'.
--
-- Note the following mapping as a guide:
--
-- +-----------------+----------------+------------------+
-- | Kind of Account | Normal Balance | Negative Balance |
-- +-----------------+----------------+------------------+
-- | Asset           | Debit          | Credit           |
-- +-----------------+----------------+------------------+
-- | Liability       | Credit         | Debit            |
-- +-----------------+----------------+------------------+
-- | Equity          | Credit         | Debit            |
-- +-----------------+----------------+------------------+
-- | Revenue         | Credit         | Debit            |
-- +-----------------+----------------+------------------+
-- | Expense         | Debit          | Credit           |
-- +-----------------+----------------+------------------+
normalSideByAccountKind :: AccountKind -> Side
normalSideByAccountKind AccountKindAsset = SideDebit
normalSideByAccountKind AccountKindLiability = SideCredit
normalSideByAccountKind AccountKindEquity = SideCredit
normalSideByAccountKind AccountKindRevenue = SideCredit
normalSideByAccountKind AccountKindExpense = SideDebit
