{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides definitions for acccounts and types of accounts as
-- they are used in accounting reporting.
module Haspara.Accounting.Account where

import qualified Data.Aeson as Aeson
import Data.Hashable (Hashable)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Haspara.Internal.Aeson (aesonOptionsForSingleTag, commonAesonOptions)


-- * Account Kind


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
-- >>> :set -XTypeApplications
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
data AccountKind
  = AccountKindAsset
  | AccountKindLiability
  | AccountKindEquity
  | AccountKindRevenue
  | AccountKindExpense
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)


instance Hashable AccountKind


instance Aeson.FromJSON AccountKind where
  parseJSON = Aeson.genericParseJSON $ aesonOptionsForSingleTag "AccountKind"


instance Aeson.ToJSON AccountKind where
  toJSON = Aeson.genericToJSON $ aesonOptionsForSingleTag "AccountKind"


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
accountKindText AccountKindAsset = "Asset"
accountKindText AccountKindLiability = "Liability"
accountKindText AccountKindEquity = "Equity"
accountKindText AccountKindRevenue = "Revenue"
accountKindText AccountKindExpense = "Expense"


-- * Account


-- | Type encoding for account values.
--
-- This definition provides both the 'AccountKind' and an arbitrary object
-- identifying the account. This arbitrary nature provides flexibility to
-- use-site to use its own account identity and accompanying information when
-- required.
--
-- >>> :set -XTypeApplications
-- >>> let acc = Account AccountKindAsset (1 ::Int)
-- >>> Data.Aeson.encode acc
-- "{\"kind\":\"ASSET\",\"object\":1}"
-- >>> Data.Aeson.decode @(Account Int) (Data.Aeson.encode acc)
-- Just (Account {accountKind = AccountKindAsset, accountObject = 1})
-- >>> Data.Aeson.decode (Data.Aeson.encode acc) == Just acc
-- True
data Account o = Account
  { accountKind :: !AccountKind
  , accountObject :: !o
  }
  deriving (Eq, Generic, Ord, Show)


instance Hashable o => Hashable (Account o)


instance Aeson.FromJSON o => Aeson.FromJSON (Account o) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "account"


instance Aeson.ToJSON o => Aeson.ToJSON (Account o) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "account"
