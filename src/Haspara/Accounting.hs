-- | This module provides a collection of definitions for a rudimentary
-- accounting functionality.

module Haspara.Accounting
  ( Account(..)
  , AccountKind(..)
  , accountKindText
  , Entry(..)
  , buildEntry
  , Event(..)
  , eventDate
  , eventObject
  , negateEvent
  , mkEvent
  , Posting(..)
  , postingEvents
  , post
  , Ledger(..)
  , LedgerItem(..)
  , mkLedger
  , addEntry
  , entryDate
  , entryObject
  , entryQuantity
  , entryDebit
  , entryCredit
  ) where


import Haspara.Accounting.Account (Account(..), AccountKind(..), accountKindText)
import Haspara.Accounting.Event   (Event(..), eventDate, eventObject, mkEvent, negateEvent)
import Haspara.Accounting.Ledger
       ( Entry(..)
       , Ledger(..)
       , LedgerItem(..)
       , Posting(..)
       , addEntry
       , buildEntry
       , entryCredit
       , entryDate
       , entryDebit
       , entryObject
       , entryQuantity
       , mkLedger
       , post
       , postingEvents
       )
