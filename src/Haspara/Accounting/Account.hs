-- | This module provides definitions for acccounts and types of accounts as
-- they are used in accounting reporting.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Haspara.Accounting.Account where

import           Data.Hashable          (Hashable)
import qualified Data.Text              as T
import qualified Deriving.Aeson         as DA
import qualified Deriving.Aeson.Stock   as DAS
import           Haspara.Internal.Aeson (UpperCase)


-- * Account Kind
-- $accountKind


-- | Type encoding for ledger account type.
--
-- This type covers both balance sheet and income statement account types:
--
-- 1. For balance sheet accounts:
--     1. Asset ('AccountKindAsset')
--     2. Liability ('AccountKindLiability')
--     3. Equity ('AccountKindEquity')
-- 2. For income statement accounts:
--     1. Revenue ('AccountKindRevenue')
--     2. Expense ('AccountKindExpense')
--
-- 'Data.Aeson.FromJSON' and 'Data.Aeson.ToJSON' instances, too:
--
-- >>> Data.Aeson.decode @AccountKind "\"ASSET\""
-- Just AccountKindAsset
-- >>> Data.Aeson.decode @AccountKind "\"LIABILITY\""
-- Just AccountKindLiability
-- >>> Data.Aeson.decode @AccountKind "\"EQUITY\""
-- Just AccountKindEquity
-- >>> Data.Aeson.decode @AccountKind "\"REVENUE\""
-- Just AccountKindRevenue
-- >>> Data.Aeson.decode @AccountKind "\"EXPENSE\""
-- Just AccountKindExpense
-- >>> Data.Aeson.encode AccountKindAsset
-- "\"ASSET\""
-- >>> Data.Aeson.encode AccountKindLiability
-- "\"LIABILITY\""
-- >>> Data.Aeson.encode AccountKindEquity
-- "\"EQUITY\""
-- >>> Data.Aeson.encode AccountKindRevenue
-- "\"REVENUE\""
-- >>> Data.Aeson.encode AccountKindExpense
-- "\"EXPENSE\""
data AccountKind =
    AccountKindAsset
  | AccountKindLiability
  | AccountKindEquity
  | AccountKindRevenue
  | AccountKindExpense
  deriving (Enum, Eq, DA.Generic, Ord, Show)
  deriving (DA.FromJSON, DA.ToJSON) via DA.CustomJSON '[DA.ConstructorTagModifier (DA.StripPrefix "AccountKind", UpperCase)] AccountKind


instance Hashable AccountKind


-- | Provides textual representation of a given 'AccountKind'.
--
-- >>> accountKindText AccountKindAsset
-- "Asset"
-- >>> accountKindText AccountKindLiability
-- "Liability"
-- >>> accountKindText AccountKindEquity
-- "Equity"
-- >>> accountKindText AccountKindRevenue
-- "Revenue"
-- >>> accountKindText AccountKindExpense
-- "Expense"
accountKindText :: AccountKind -> T.Text
accountKindText AccountKindAsset     = "Asset"
accountKindText AccountKindLiability = "Liability"
accountKindText AccountKindEquity    = "Equity"
accountKindText AccountKindRevenue   = "Revenue"
accountKindText AccountKindExpense   = "Expense"


-- * Account
-- $account


-- | Type encoding for account values.
--
-- This definition provides both the 'AccountKind' and an arbitrary object
-- identifying the account. This arbitrary nature provides flexibility to
-- use-site to use its own account identity and accompanying information when
-- required.
--
-- >>> let acc = Account AccountKindAsset (1 ::Int)
-- >>> Data.Aeson.encode acc
-- "{\"kind\":\"ASSET\",\"object\":1}"
-- >>> Data.Aeson.decode @(Account Int) (Data.Aeson.encode acc)
-- Just (Account {accountKind = AccountKindAsset, accountObject = 1})
-- >>> Data.Aeson.decode (Data.Aeson.encode acc) == Just acc
-- True
data Account o = Account
  { accountKind   :: !AccountKind
  , accountObject :: !o
  }
  deriving (Eq, DAS.Generic, Ord, Show)
  deriving (DAS.FromJSON, DAS.ToJSON) via DAS.PrefixedSnake "account" (Account o)


instance Hashable o => Hashable (Account o)
