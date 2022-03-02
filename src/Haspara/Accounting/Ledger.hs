{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Haspara.Accounting.Ledger where

import Deriving.Aeson             (CustomJSON(CustomJSON), FromJSON, Generic, ToJSON)
import Deriving.Aeson.Stock       (PrefixedSnake)
import GHC.TypeLits               (KnownNat, Nat)
import Haspara                    (Quantity)
import Haspara.Accounting.Account (Account)
import Haspara.Accounting.Entry   (Entry(..), entryQuantity)


data Ledger a o (s :: Nat) = Ledger
  { ledgerAccount :: !(Account a)
  , ledgerOpening :: !(Quantity s)
  , ledgerClosing :: !(Quantity s)
  , ledgerRunning :: ![LedgerItem o s]
  } deriving (Eq, Generic, Ord, Show)
  deriving (FromJSON, ToJSON)
  via PrefixedSnake "ledger" (Ledger a o s)


data LedgerItem o (s :: Nat) = LedgerItem
  { ledgerItemEntry   :: !(Entry o s)
  , ledgerItemBalance :: !(Quantity s)
  } deriving (Eq, Generic, Ord, Show)
  deriving (FromJSON, ToJSON)
  via PrefixedSnake "ledgerItem" (LedgerItem o s)


mkLedger :: KnownNat s => Account a -> Quantity s -> [Entry o s] -> Ledger a o s
mkLedger a o = foldl addEntry (Ledger a o o [])


addEntry :: KnownNat s => Ledger a o s -> Entry o s -> Ledger a o s
addEntry l@(Ledger _ _ c r) e = l { ledgerClosing = balance, ledgerRunning = r <> [item]}
  where
    balance = c + entryQuantity e
    item = LedgerItem e balance
