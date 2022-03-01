{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Haspara.Accounting.Entry where

import           Data.Aeson                     ((.:), (.=))
import qualified Data.Aeson                     as Aeson
import qualified Data.Char                      as C
import qualified Data.Text                      as T
import           Data.Time                      (Day)
import           GHC.TypeLits                   (KnownNat, Nat)
import qualified Haspara                        as H
import           Haspara.Accounting.AccountKind (AccountKind(..))
import           Haspara.Accounting.Event       (Event(..))
import           Haspara.Accounting.Types       (UnsignedQuantity)
import           Refined                        (unrefine)


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


entryDate :: KnownNat s => Entry o s -> Day
entryDate (EntryDebit d _ _)  = d
entryDate (EntryCredit d _ _) = d


entryQuantity :: KnownNat s => Entry o s -> H.Quantity s
entryQuantity (EntryDebit _ _ q)  = unrefine q
entryQuantity (EntryCredit _ _ q) = -(unrefine q)


entryObject :: KnownNat s => Entry o s -> o
entryObject (EntryDebit _ o _)  = o
entryObject (EntryCredit _ o _) = o


entryDebit :: KnownNat s => Entry o s -> Maybe (UnsignedQuantity s)
entryDebit (EntryDebit _ _ x) = Just x
entryDebit EntryCredit {}     = Nothing


entryCredit :: KnownNat s => Entry o s -> Maybe (UnsignedQuantity s)
entryCredit EntryDebit {}       = Nothing
entryCredit (EntryCredit _ _ x) = Just x


-- |
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
