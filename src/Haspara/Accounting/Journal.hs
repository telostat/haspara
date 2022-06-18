{-# LANGUAGE DataKinds #-}

module Haspara.Accounting.Journal where

import qualified Data.Text                  as T
import           Data.Time                  (Day)
import           GHC.TypeLits               (KnownNat, Nat)
import           Haspara.Accounting.Account (Account(accountKind))
import           Haspara.Accounting.Side    (Side(..), sideByAccountKind)
import           Haspara.Quantity           (Quantity, UnsignedQuantity, absQuantity, sumUnsignedQuantity)


newtype Journal (precision :: Nat) account event = Journal
  { journalEntries :: [JournalEntry precision account event]
  }


data JournalEntry (precision :: Nat) account event = JournalEntry
  { journalEntryId          :: !T.Text
  , journalEntryDate        :: !Day
  , journalEntryItems       :: ![JournalEntryItem precision account event]
  , journalEntryDescription :: !T.Text
  }


journalEntryTotalDebit
  :: KnownNat precision
  => JournalEntry precision account event
  -> UnsignedQuantity precision
journalEntryTotalDebit =
    sumUnsignedQuantity
  . fmap journalEntryItemAmount
  . filter ((==) SideDebit . journalEntryItemSide)
  . journalEntryItems


journalEntryTotalCredit
  :: KnownNat precision
  => JournalEntry precision account event
  -> UnsignedQuantity precision
journalEntryTotalCredit =
    sumUnsignedQuantity
  . fmap journalEntryItemAmount
  . filter ((==) SideCredit . journalEntryItemSide)
  . journalEntryItems


isJournalEntryBalanced
  :: KnownNat precision
  => JournalEntry precision account event
  -> Bool
isJournalEntryBalanced = (==)
  <$> journalEntryTotalDebit
  <*> journalEntryTotalCredit


data JournalEntryItem (precision :: Nat) account event = JournalEntryItem
  { journalEntryItemSide    :: !Side
  , journalEntryItemAmount  :: !(UnsignedQuantity precision)
  , journalEntryItemAccount :: !(Account account)
  , journalEntryItemEvent   :: !event
  }


mkJournalEntryItem
  :: KnownNat precision
  => Quantity precision
  -> Account account
  -> event
  -> JournalEntryItem precision account event
mkJournalEntryItem qty acc evt =
  JournalEntryItem
    { journalEntryItemSide = sideByAccountKind (accountKind acc) qty
    , journalEntryItemAmount = absQuantity qty
    , journalEntryItemAccount = acc
    , journalEntryItemEvent = evt
    }
