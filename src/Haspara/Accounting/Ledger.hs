-- | This module provides definitions for postings, ledgers and ledger entries.

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Haspara.Accounting.Ledger where

import           Data.Aeson                 ((.:), (.=))
import qualified Data.Aeson                 as Aeson
import qualified Data.Char                  as C
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text                  as T
import           Data.Time                  (Day)
import           Deriving.Aeson             (CustomJSON(CustomJSON), FromJSON, Generic, ToJSON)
import           Deriving.Aeson.Stock       (PrefixedSnake, Vanilla)
import           GHC.TypeLits               (KnownNat, Nat)
import           Haspara.Accounting.Account (Account(accountKind), AccountKind(..))
import           Haspara.Accounting.Event   (Event(..), eventObject)
import           Haspara.Quantity           (Quantity, UnsignedQuantity)
import           Refined                    (unrefine)


-- | Type encoding of a ledger.
data Ledger a o (s :: Nat) = Ledger
  { ledgerAccount :: !(Account a)
  , ledgerOpening :: !(Quantity s)
  , ledgerClosing :: !(Quantity s)
  , ledgerRunning :: ![LedgerItem o s]
  } deriving (Eq, Generic, Ord, Show)
  deriving (FromJSON, ToJSON) via PrefixedSnake "ledger" (Ledger a o s)


-- | Type encoding of a ledger item.
data LedgerItem o (s :: Nat) = LedgerItem
  { ledgerItemEntry   :: !(Entry o s)
  , ledgerItemBalance :: !(Quantity s)
  } deriving (Eq, Generic, Ord, Show)
  deriving (FromJSON, ToJSON)
  via PrefixedSnake "ledgerItem" (LedgerItem o s)


-- | Creates a ledger from a given list of 'Entry' values.
mkLedger :: KnownNat s => Account a -> Quantity s -> [Entry o s] -> Ledger a o s
mkLedger a o = foldl addEntry (Ledger a o o [])


-- | Adds a new entry to a ledger.
addEntry :: KnownNat s => Ledger a o s -> Entry o s -> Ledger a o s
addEntry l@(Ledger _ _ c r) e = l { ledgerClosing = balance, ledgerRunning = r <> [item]}
  where
    balance = c + entryQuantity e
    item = LedgerItem e balance


-- | Type encoding for a posting.
--
-- >>> :set -XDataKinds
-- >>> import Haspara.Accounting
-- >>> import Refined
-- >>> import qualified Data.Aeson as Aeson
-- >>> import qualified Data.List.NonEmpty as NE
-- >>> let date = read "2021-01-01"
-- >>> let oid = 1 :: Int
-- >>> let qty = $$(refineTH 42) :: UnsignedQuantity 2
-- >>> let event = EventDecrement date oid qty
-- >>> let account = Account AccountKindAsset ("Cash" :: String, 1 ::Int)
-- >>> let posting =  Posting . NE.fromList $ [(event, account)]
-- >>> let json = Aeson.encode posting
-- >>> json
-- "[[{\"qty\":42.0,\"type\":\"DECREMENT\",\"obj\":1,\"date\":\"2021-01-01\"},{\"kind\":\"ASSET\",\"object\":[\"Cash\",1]}]]"
-- >>> Aeson.decode json :: Maybe (Posting (String, Int) Int 2)
-- Just (Posting ((EventDecrement 2021-01-01 1 (Refined 42.00),Account {accountKind = AccountKindAsset, accountObject = ("Cash",1)}) :| []))
-- >>> Aeson.decode json == Just posting
-- True
newtype Posting a o (s :: Nat) = Posting (NE.NonEmpty (Event o s, Account a))
  deriving (Eq, Generic, Ord, Show)
  deriving (FromJSON, ToJSON)
  via Vanilla (Posting a o s)


-- | Returns the list of posting event sources.
postingEvents :: (KnownNat s) => Posting a o s -> [o]
postingEvents (Posting es)  = eventObject . fst <$> NE.toList es


-- | Posts an event.
post :: (KnownNat s) => Posting a o s -> [(Account a, Entry o s)]
post (Posting xs)       = go (NE.toList xs)
  where
    go []              = []
    go ((ev, ac) : ys) = (ac, buildEntry ev (accountKind ac)) : go ys


-- | Encoding of a posting entry.
--
-- >>> :set -XDataKinds
-- >>> import Refined
-- >>> let date = read "2021-01-01"
-- >>> let oid = 1 :: Int
-- >>> let qty = $$(refineTH 42) :: UnsignedQuantity 2
-- >>> let entry = EntryDebit date oid qty
-- >>> let json = Aeson.encode entry
-- >>> json
-- "{\"qty\":42.0,\"type\":\"DEBIT\",\"obj\":1,\"date\":\"2021-01-01\"}"
-- >>> Aeson.decode json :: Maybe (Entry Int 2)
-- Just (EntryDebit 2021-01-01 1 (Refined 42.00))
-- >>> Aeson.decode json == Just entry
-- True
data Entry o (s :: Nat) =
    EntryDebit Day o (UnsignedQuantity s)
  | EntryCredit Day o (UnsignedQuantity s)
  deriving (Eq, Ord, Show)


instance (Aeson.FromJSON o, KnownNat s) => Aeson.FromJSON (Entry o s) where
  parseJSON = Aeson.withObject "Entry" $ \o -> do
    dorc <- o .: "type"
    cons <- case T.map C.toUpper dorc of
      "DEBIT"  -> pure EntryDebit
      "CREDIT" -> pure EntryCredit
      x        -> fail ("Unknown entry type: " <> T.unpack x)
    date <- o .: "date"
    obj <- o .: "obj"
    qty <- o .: "qty"
    pure (cons date obj qty)


instance (Aeson.ToJSON o, KnownNat s) => Aeson.ToJSON (Entry o s) where
  toJSON x = case x of
    EntryDebit d o q  -> Aeson.object ["type" .= ("DEBIT" :: T.Text), "date" .= d, "obj" .= o, "qty" .= q]
    EntryCredit d o q -> Aeson.object ["type" .= ("CREDIT" :: T.Text), "date" .= d, "obj" .= o, "qty" .= q]


-- | Returns the date of the posting entry.
entryDate :: KnownNat s => Entry o s -> Day
entryDate (EntryDebit d _ _)  = d
entryDate (EntryCredit d _ _) = d


-- | Returns the quantity of the posting entry.
entryQuantity :: KnownNat s => Entry o s -> Quantity s
entryQuantity (EntryDebit _ _ q)  = unrefine q
entryQuantity (EntryCredit _ _ q) = -(unrefine q)


-- | Returns the source object of the posting entry.
entryObject :: KnownNat s => Entry o s -> o
entryObject (EntryDebit _ o _)  = o
entryObject (EntryCredit _ o _) = o


-- | Returns the debit quantity of the posting entry.
entryDebit :: KnownNat s => Entry o s -> Maybe (UnsignedQuantity s)
entryDebit (EntryDebit _ _ x) = Just x
entryDebit EntryCredit {}     = Nothing


-- | Returns the credit quantity of the posting entry.
entryCredit :: KnownNat s => Entry o s -> Maybe (UnsignedQuantity s)
entryCredit EntryDebit {}       = Nothing
entryCredit (EntryCredit _ _ x) = Just x


-- | Consumes an event and a type of account, and produces a posting entry.
--
-- Note the following map as a guide:
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
buildEntry :: (KnownNat s) => Event o s -> AccountKind -> Entry o s
buildEntry (EventDecrement d o x) AccountKindAsset     = EntryCredit d o x
buildEntry (EventIncrement d o x) AccountKindAsset     = EntryDebit  d o x
buildEntry (EventDecrement d o x) AccountKindLiability = EntryDebit  d o x
buildEntry (EventIncrement d o x) AccountKindLiability = EntryCredit d o x
buildEntry (EventDecrement d o x) AccountKindEquity    = EntryDebit  d o x
buildEntry (EventIncrement d o x) AccountKindEquity    = EntryCredit d o x
buildEntry (EventDecrement d o x) AccountKindRevenue   = EntryDebit  d o x
buildEntry (EventIncrement d o x) AccountKindRevenue   = EntryCredit d o x
buildEntry (EventDecrement d o x) AccountKindExpense   = EntryCredit d o x
buildEntry (EventIncrement d o x) AccountKindExpense   = EntryDebit  d o x
