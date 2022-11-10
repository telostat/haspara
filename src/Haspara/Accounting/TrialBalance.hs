{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This module provides data definitions and functions for trial balances.
module Haspara.Accounting.TrialBalance where

import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat)
import Haspara.Accounting.Amount (Amount)
import Haspara.Accounting.Balance (Balance (Balance, balanceSide), amountFromBalance, updateBalance)
import Haspara.Accounting.Ledger (GeneralLedger (generalLedgerLedgers), Ledger, ledgerClosing)
import Haspara.Accounting.Side (Side (..))
import Haspara.Internal.Aeson (commonAesonOptions)


-- | Data definition for a trial balance.
newtype TrialBalance (precision :: Nat) account event = TrialBalance
  { trialBalanceItems :: [TrialBalanceItem precision account event]
  }
  deriving (Eq, Generic, Show)


instance (KnownNat precision, Aeson.FromJSON account, Aeson.FromJSON event) => Aeson.FromJSON (TrialBalance precision account event) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "trialBalance"


instance (KnownNat precision, Aeson.ToJSON account, Aeson.ToJSON event) => Aeson.ToJSON (TrialBalance precision account event) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "trialBalance"


-- | Data definition for a trial balance item.
data TrialBalanceItem (precision :: Nat) account event = TrialBalanceItem
  { trialBalanceItemLedger :: !(Ledger precision account event)
  , trialBalanceItemBalance :: !(Balance precision)
  }
  deriving (Eq, Generic, Show)


instance (KnownNat precision, Aeson.FromJSON account, Aeson.FromJSON event) => Aeson.FromJSON (TrialBalanceItem precision account event) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "trialBalanceItem"


instance (KnownNat precision, Aeson.ToJSON account, Aeson.ToJSON event) => Aeson.ToJSON (TrialBalanceItem precision account event) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "trialBalanceItem"


-- | Returns the amount of the trial balance item. This is a simple conversion
-- from 'Balance' to 'Amount'.
trialBalanceItemAmount
  :: KnownNat precision
  => TrialBalanceItem precision account event
  -> Amount precision
trialBalanceItemAmount = amountFromBalance . trialBalanceItemBalance


-- | Given a general ledger, prepares the trial balance.
prepareTrialBalance
  :: KnownNat precision
  => GeneralLedger precision account event
  -> TrialBalance precision account event
prepareTrialBalance = TrialBalance . fmap mkTrialBalanceItem . generalLedgerLedgers


-- | Converts a 'Ledger' to a 'TrialBalanceItem'.
mkTrialBalanceItem
  :: KnownNat precision
  => Ledger precision account event
  -> TrialBalanceItem precision account event
mkTrialBalanceItem ledger =
  TrialBalanceItem ledger (ledgerClosing ledger)


-- | Computes the trial balance totals as a 2-tuple of total debits and total
-- credits.
trialBalanceTotals
  :: KnownNat precision
  => TrialBalance precision account event
  -> (Balance precision, Balance precision)
trialBalanceTotals (TrialBalance items) =
  let itemsFromDb = amountFromBalance . trialBalanceItemBalance <$> filter ((==) SideDebit . balanceSide . trialBalanceItemBalance) items
      itemsFromCr = amountFromBalance . trialBalanceItemBalance <$> filter ((==) SideCredit . balanceSide . trialBalanceItemBalance) items
      totalDb = foldl updateBalance (Balance SideDebit 0) itemsFromDb
      totalCr = foldl updateBalance (Balance SideCredit 0) itemsFromCr
   in (totalDb, totalCr)
