-- | This module provides data definitions and functions to work with journal
-- entries.

{-# LANGUAGE DataKinds #-}

module Haspara.Accounting.Journal where

import qualified Data.Text                  as T
import           Data.Time                  (Day)
import           GHC.Generics               (Generic)
import           GHC.TypeLits               (KnownNat, Nat)
import           Haspara.Accounting.Account (Account(accountKind))
import           Haspara.Accounting.Amount  (Amount(Amount, amountSide, amountValue))
import           Haspara.Accounting.Side    (Side(..), sideByAccountKind)
import           Haspara.Quantity           (Quantity, UnsignedQuantity, absQuantity, sumUnsignedQuantity)


-- | Data definition for the journal entries of interest (like a general
-- ledger.)
--
-- A 'Journal' is a list of 'JournalEntry' records which are polymorphic over
-- the precision of the monetary quantities, the account and event objects.
newtype Journal (precision :: Nat) account event = Journal
  { journalEntries :: [JournalEntry precision account event]
  }
  deriving (Show)


-- | Data definition for a journal entry.
--
-- A journal entry has a (unique) identifier, date and description, and a list
-- of 'JournalEntryItem's. Journal entry definition is polymorphic over the
-- precision of the monetary quantities, the account and event objects.
data JournalEntry (precision :: Nat) account event = JournalEntry
  { journalEntryId          :: !T.Text
  , journalEntryDate        :: !Day
  , journalEntryItems       :: ![JournalEntryItem precision account event]
  , journalEntryDescription :: !T.Text
  }
  deriving (Show)


-- | Returns the total debit amount of a journal entry.
journalEntryTotalDebit
  :: KnownNat precision
  => JournalEntry precision account event
  -> UnsignedQuantity precision
journalEntryTotalDebit =
    sumUnsignedQuantity
  . fmap (amountValue . journalEntryItemAmount)
  . filter ((==) SideDebit . amountSide . journalEntryItemAmount)
  . journalEntryItems


-- | Returns the total credit amount of a journal entry.
journalEntryTotalCredit
  :: KnownNat precision
  => JournalEntry precision account event
  -> UnsignedQuantity precision
journalEntryTotalCredit =
    sumUnsignedQuantity
  . fmap (amountValue . journalEntryItemAmount)
  . filter ((==) SideCredit . amountSide . journalEntryItemAmount)
  . journalEntryItems


-- | Predicate to check if a journal entry is balanced or not.
--
-- The logical check is indeed whether the total debit amount is equal to the
-- total credit amount or not.
isJournalEntryBalanced
  :: KnownNat precision
  => JournalEntry precision account event
  -> Bool
isJournalEntryBalanced = (==)
  <$> journalEntryTotalDebit
  <*> journalEntryTotalCredit


-- | Data definition for a journal entry item.
--
-- A journal entry item has a 'Side', an unsigned quantity as amount, an account
-- that it belongs to and the event the item is originating from. Journal entry
-- item definition is polymorphic over the precision of the monetary quantities,
-- the account and event objects.
data JournalEntryItem (precision :: Nat) account event = JournalEntryItem
  { journalEntryItemAmount  :: !(Amount precision)
  , journalEntryItemAccount :: !(Account account)
  , journalEntryItemEvent   :: !event
  }
  deriving (Eq, Generic, Show)


-- | Creates a 'JournalEntryItem' from the given signed quantity, the account it
-- belongs to and the event it is originating from.
--
-- The 'Side' is resolved from the sign of the quantity and the account the item
-- belongs to.
mkJournalEntryItem
  :: KnownNat precision
  => Quantity precision
  -> Account account
  -> event
  -> JournalEntryItem precision account event
mkJournalEntryItem qty acc evt =
  JournalEntryItem
    { journalEntryItemAmount = Amount (sideByAccountKind (accountKind acc) qty) (absQuantity qty)
    , journalEntryItemAccount = acc
    , journalEntryItemEvent = evt
    }
