{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides data definitions and functions for ledgers and
-- postings.
module Haspara.Accounting.Ledger where

import qualified Data.Aeson as Aeson
import Data.Default (def)
import Data.Foldable (foldl')
import qualified Data.Map.Strict as HM
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import qualified Data.Text as T
import Data.Time (Day)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat)
import Haspara.Accounting.Account (Account (accountKind))
import Haspara.Accounting.Amount (Amount, amountFromQuantity, amountFromValue)
import Haspara.Accounting.Balance (Balance (Balance), updateBalance, updateBalanceWithInventory)
import Haspara.Accounting.Inventory (InventoryHistoryItem (MkInventoryHistoryItem, inventoryHistoryItemPnl), updateInventoryVV)
import Haspara.Accounting.Journal (JournalEntry (..), JournalEntryItem (..), JournalEntryItemInventoryEvent (JournalEntryItemInventoryEvent))
import Haspara.Accounting.Side (normalSideByAccountKind)
import Haspara.Internal.Aeson (commonAesonOptions)
import Haspara.Quantity (Quantity)


-- | Data definition for a general ledger.
newtype GeneralLedger (precision :: Nat) account event = GeneralLedger
  { generalLedgerLedgers :: [Ledger precision account event]
  }
  deriving (Eq, Generic, Show)


instance (KnownNat precision, Aeson.FromJSON account, Aeson.FromJSON event) => Aeson.FromJSON (GeneralLedger precision account event) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "generalLedger"


instance (KnownNat precision, Aeson.ToJSON account, Aeson.ToJSON event) => Aeson.ToJSON (GeneralLedger precision account event) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "generalLedger"


-- | Data definition for a ledger.
data Ledger (precision :: Nat) account event = Ledger
  { ledgerAccount :: !(Account account)
  , ledgerOpening :: !(Balance precision)
  , ledgerRunning :: ![LedgerEntry precision event]
  }
  deriving (Eq, Generic, Show)


instance (KnownNat precision, Aeson.FromJSON account, Aeson.FromJSON event) => Aeson.FromJSON (Ledger precision account event) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "ledger"


instance (KnownNat precision, Aeson.ToJSON account, Aeson.ToJSON event) => Aeson.ToJSON (Ledger precision account event) where
  -- TODO: Add ledgerClosing, too.
  toJSON = Aeson.genericToJSON $ commonAesonOptions "ledger"


-- | Returns the closing balance of a ledger.
ledgerClosing
  :: KnownNat precision
  => Ledger precision account event
  -> Balance precision
ledgerClosing ledger = maybe (ledgerOpening ledger) ledgerEntryBalance (listToMaybe (ledgerRunning ledger))


-- | Type encoding of a ledger item.
data LedgerEntry (precision :: Nat) event = LedgerEntry
  { ledgerEntryDate :: !Day
  , ledgerEntryAmount :: !(Amount precision)
  , ledgerEntryDescription :: !T.Text
  , ledgerEntryEvent :: !event
  , ledgerEntryPostingId :: !T.Text
  , ledgerEntryBalance :: !(Balance precision)
  }
  deriving (Eq, Generic, Show)


instance (KnownNat precision, Aeson.FromJSON event) => Aeson.FromJSON (LedgerEntry precision event) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "ledgerEntry"


instance (KnownNat precision, Aeson.ToJSON event) => Aeson.ToJSON (LedgerEntry precision event) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "ledgerEntry"


-- | Initializes an empty ledger for a given account.
initLedger
  :: KnownNat precision
  => Account account
  -> Ledger precision account event
initLedger acc = Ledger acc balance []
  where
    balance = Balance (normalSideByAccountKind (accountKind acc)) 0 def


-- | Initializes a ledger with the given opening balance.
initLedgerWithOpeningBalance
  :: KnownNat precision
  => Account account
  -> Balance precision
  -> Ledger precision account event
initLedgerWithOpeningBalance acc balance = Ledger acc balance []


-- | Initializes a ledger with the given opening value.
--
-- See 'amountFromValue' for the meaning of the concept of value.
initLedgerWithOpeningValue
  :: KnownNat precision
  => Account account
  -> (Maybe (Quantity 12), Quantity precision)
  -> Ledger precision account event
initLedgerWithOpeningValue acc (mstk, qty) = initLedgerWithOpeningBalance acc balance
  where
    inventory = snd $ case mstk of
      Nothing -> def
      Just sq -> updateInventoryVV (read "1970-01-01") qty sq def
    amount = amountFromValue (accountKind acc) qty
    balance0 = Balance (normalSideByAccountKind (accountKind acc)) 0 inventory
    balance = updateBalance balance0 amount


-- | Initializes a ledger with the given opening quantity.
--
-- See 'amountFromQuantity' for the meaning of the concept of quantity.
initLedgerWithOpeningQuantity
  :: KnownNat precision
  => Account account
  -> (Maybe (Quantity 12), Quantity precision)
  -> Ledger precision account event
initLedgerWithOpeningQuantity acc (mstk, qty) = initLedgerWithOpeningBalance acc balance
  where
    inventory = snd $ case mstk of
      Nothing -> def
      Just sq -> updateInventoryVV (read "1970-01-01") qty sq def
    amount = amountFromQuantity (accountKind acc) qty
    balance0 = Balance (normalSideByAccountKind (accountKind acc)) 0 inventory
    balance = updateBalance balance0 amount


-- | Posts a given list of journal entries to the given general ledger and
-- returns the new general ledger.
postEntries
  :: KnownNat precision
  => Eq account
  => Ord account
  => GeneralLedger precision account event
  -> [JournalEntry precision account event]
  -> GeneralLedger precision account event
postEntries = foldl postEntry


-- | Posts a given journal entry to the given general ledger and returns the new
-- general ledger.
postEntry
  :: KnownNat precision
  => Eq account
  => Ord account
  => GeneralLedger precision account event
  -> JournalEntry precision account event
  -> GeneralLedger precision account event
postEntry gl je = foldl (`postEntryItem` je) gl (journalEntryItems je)


-- | Posts a given journal entry item of a given journal entry to the given
-- general ledger and returns the new general ledger.
postEntryItem
  :: KnownNat precision
  => Eq account
  => Ord account
  => GeneralLedger precision account event
  -> JournalEntry precision account event
  -> JournalEntryItem precision account event
  -> GeneralLedger precision account event
postEntryItem gl je (JournalEntryItem amt acc evt invevt) =
  let ledgers = generalLedgerLedgers gl
      ledgersDb = HM.fromList $ zip (fmap ledgerAccount ledgers) ledgers
      ledgerCurr = fromMaybe (initLedger acc) $ HM.lookup acc ledgersDb
      jeDate = journalEntryDate je
      jeDesc = journalEntryDescription je
      jeCode = journalEntryId je
      (jePnl, ledgerNext) = postItem ledgerCurr jeDate amt jeDesc evt jeCode invevt
      ledgersDbNext = HM.insert acc ledgerNext ledgersDb
      ngl =
        GeneralLedger
          { generalLedgerLedgers = HM.elems ledgersDbNext
          }
   in postEntries ngl (maybeToList jePnl)


-- | Performs a posting to the given ledger.
postItem
  :: KnownNat precision
  => Ledger precision account event
  -> Day
  -> Amount precision
  -> T.Text
  -> event
  -> T.Text
  -> Maybe (JournalEntryItemInventoryEvent account event)
  -> (Maybe (JournalEntry precision account event), Ledger precision account event)
postItem ledger date amt dsc evt pid Nothing =
  let balanceLast = ledgerClosing ledger
      balanceNext = updateBalance balanceLast amt
      item =
        LedgerEntry
          { ledgerEntryDate = date
          , ledgerEntryAmount = amt
          , ledgerEntryDescription = dsc
          , ledgerEntryEvent = evt
          , ledgerEntryPostingId = pid
          , ledgerEntryBalance = balanceNext
          }
   in ( Nothing
      , ledger
          { ledgerRunning = item : ledgerRunning ledger
          }
      )
postItem ledger date amt dsc evt pid (Just (JournalEntryItemInventoryEvent pnlacc pnlevt evtqty)) =
  let balanceLast = ledgerClosing ledger
      (histitems, balanceNext) = updateBalanceWithInventory date balanceLast amt evtqty
      item =
        LedgerEntry
          { ledgerEntryDate = date
          , ledgerEntryAmount = amt
          , ledgerEntryDescription = dsc
          , ledgerEntryEvent = evt
          , ledgerEntryPostingId = pid
          , ledgerEntryBalance = balanceNext
          }
      mje = case histitems of
        [] -> Nothing
        _ ->
          Just $
            JournalEntry
              { journalEntryId = pid
              , journalEntryDate = date
              , journalEntryItems = foldl' (\a c -> a <> histItemToJournalEntryItem pnlevt (ledgerAccount ledger) pnlacc c) [] histitems
              , journalEntryDescription = "Realized PnL due to: " <> dsc
              }
   in ( mje
      , ledger
          { ledgerRunning = item : ledgerRunning ledger
          }
      )


-- | Creates 2 journal entry items for the captured non-zero PnL.
histItemToJournalEntryItem
  :: KnownNat precision
  => event
  -> Account account
  -> Account account
  -> InventoryHistoryItem 8 12 precision
  -> [JournalEntryItem precision account event]
histItemToJournalEntryItem event accAsset accRevenue MkInventoryHistoryItem {..} =
  if inventoryHistoryItemPnl == 0
    then []
    else
      [ JournalEntryItem
          { journalEntryItemAmount = amountFromQuantity (accountKind accAsset) inventoryHistoryItemPnl
          , journalEntryItemAccount = accAsset
          , journalEntryItemEvent = event
          , journalEntryItemInventoryEvent = Nothing
          }
      , JournalEntryItem
          { journalEntryItemAmount = amountFromQuantity (accountKind accRevenue) inventoryHistoryItemPnl
          , journalEntryItemAccount = accRevenue
          , journalEntryItemEvent = event
          , journalEntryItemInventoryEvent = Nothing
          }
      ]
