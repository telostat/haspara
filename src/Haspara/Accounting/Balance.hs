{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

-- | This module provides definitions for balances used as in accounting.
module Haspara.Accounting.Balance where

import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import Data.Time (Day)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat)
import Haspara.Accounting.Account (AccountKind (AccountKindAsset))
import Haspara.Accounting.Amount (Amount (Amount), quantityFromAmount, valueFromAmount)
import Haspara.Accounting.Inventory (Inventory, InventoryHistoryItem, updateInventoryVV)
import Haspara.Accounting.Side (Side (..), otherSide)
import Haspara.Internal.Aeson (commonAesonOptions)
import Haspara.Quantity (Quantity, absQuantity)
import Refined (unrefine)


-- | Data definition for balances.
--
-- This definition is similar to 'Haspara.Accounting.Amount.Amount', however,
-- the value is allowed to be negative to reflect "Negative Balance" phenomenon.
--
-- See https://www.accountingtools.com/articles/what-is-a-negative-balance.html
data Balance (precision :: Nat) = Balance
  { balanceSide :: !Side
  , balanceValue :: !(Quantity precision)
  , balanceInventory :: !(Inventory 8 12 precision)
  }
  deriving (Eq, Generic, Show)


-- | 'Aeson.FromJSON' instance for 'Balance'.
--
-- For normal balances:
--
-- >>> :set -XDataKinds
-- >>> :set -XOverloadedStrings
-- >>> Aeson.eitherDecode "{\"side\": \"db\", \"value\": 42, \"inventory\": {\"current\": [], \"history\": [], \"total\": 0.0}}" :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideDebit, balanceValue = 42.00, balanceInventory = MkInventory {inventoryTotal = 0.000000000000, inventoryCurrent = fromList [], inventoryHistory = fromList []}})
-- >>> Aeson.eitherDecode "{\"side\": \"cr\", \"value\": 42, \"inventory\": {\"current\": [], \"history\": [], \"total\": 0.0}}" :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideCredit, balanceValue = 42.00, balanceInventory = MkInventory {inventoryTotal = 0.000000000000, inventoryCurrent = fromList [], inventoryHistory = fromList []}})
--
-- For negative balances:
--
-- >>> Aeson.eitherDecode "{\"side\": \"db\", \"value\": -42, \"inventory\": {\"current\": [], \"history\": [], \"total\": 0.0}}" :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideDebit, balanceValue = -42.00, balanceInventory = MkInventory {inventoryTotal = 0.000000000000, inventoryCurrent = fromList [], inventoryHistory = fromList []}})
-- >>> Aeson.eitherDecode "{\"side\": \"cr\", \"value\": -42, \"inventory\": {\"current\": [], \"history\": [], \"total\": 0.0}}" :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideCredit, balanceValue = -42.00, balanceInventory = MkInventory {inventoryTotal = 0.000000000000, inventoryCurrent = fromList [], inventoryHistory = fromList []}})
instance KnownNat precision => Aeson.FromJSON (Balance precision) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "balance"


-- | 'Aeson.ToJSON' instance for 'Balance'.
--
-- For normal balances:
--
-- >>> :set -XDataKinds
-- >>> import Data.Default (def)
-- >>> import Haspara.Accounting.Side
-- >>> import Haspara.Quantity
-- >>> Aeson.encode (Balance SideDebit (mkQuantity 42 :: Quantity 2) def)
-- "{\"side\":\"db\",\"value\":42.0,\"inventory\":{\"total\":0.0,\"current\":[],\"history\":[]}}"
-- >>> Aeson.encode (Balance SideCredit (mkQuantity 42 :: Quantity 2) def)
-- "{\"side\":\"cr\",\"value\":42.0,\"inventory\":{\"total\":0.0,\"current\":[],\"history\":[]}}"
-- >>> Aeson.eitherDecode (Aeson.encode (Balance SideDebit (mkQuantity 42 :: Quantity 2) def)) :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideDebit, balanceValue = 42.00, balanceInventory = MkInventory {inventoryTotal = 0.000000000000, inventoryCurrent = fromList [], inventoryHistory = fromList []}})
-- >>> Aeson.eitherDecode (Aeson.encode (Balance SideCredit (mkQuantity 42 :: Quantity 2) def)) :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideCredit, balanceValue = 42.00, balanceInventory = MkInventory {inventoryTotal = 0.000000000000, inventoryCurrent = fromList [], inventoryHistory = fromList []}})
--
-- For negative balances:
--
-- >>> Aeson.encode (Balance SideDebit (mkQuantity (-42) :: Quantity 2) def)
-- "{\"side\":\"db\",\"value\":-42.0,\"inventory\":{\"total\":0.0,\"current\":[],\"history\":[]}}"
-- >>> Aeson.encode (Balance SideCredit (mkQuantity (-42) :: Quantity 2) def)
-- "{\"side\":\"cr\",\"value\":-42.0,\"inventory\":{\"total\":0.0,\"current\":[],\"history\":[]}}"
-- >>> Aeson.eitherDecode (Aeson.encode (Balance SideDebit (mkQuantity (-42) :: Quantity 2) def)) :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideDebit, balanceValue = -42.00, balanceInventory = MkInventory {inventoryTotal = 0.000000000000, inventoryCurrent = fromList [], inventoryHistory = fromList []}})
-- >>> Aeson.eitherDecode (Aeson.encode (Balance SideCredit (mkQuantity (-42) :: Quantity 2) def)) :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideCredit, balanceValue = -42.00, balanceInventory = MkInventory {inventoryTotal = 0.000000000000, inventoryCurrent = fromList [], inventoryHistory = fromList []}})
instance KnownNat precision => Aeson.ToJSON (Balance precision) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "balance"
  toEncoding = Aeson.genericToEncoding $ commonAesonOptions "balance"


-- | Returns the debit quantity, if any.
balanceDebit
  :: KnownNat precision
  => Balance precision
  -> Maybe (Quantity precision)
balanceDebit (Balance SideDebit v _) = Just v
balanceDebit _ = Nothing


-- | Returns the credit quantity, if any.
balanceCredit
  :: KnownNat precision
  => Balance precision
  -> Maybe (Quantity precision)
balanceCredit (Balance SideCredit v _) = Just v
balanceCredit _ = Nothing


-- | Updates the balance with the given amount.
--
-- >>> :set -XDataKinds
-- >>> import Data.Default (def)
-- >>> import Haspara.Accounting.Amount
-- >>> import Haspara.Accounting.Side
-- >>> import Refined.Unsafe
-- >>> let balance = Balance SideDebit 42 def :: Balance 2
-- >>> balance
-- Balance {balanceSide = SideDebit, balanceValue = 42.00, balanceInventory = MkInventory {inventoryTotal = 0.000000000000, inventoryCurrent = fromList [], inventoryHistory = fromList []}}
-- >>> let amountDebit = Amount SideDebit (unsafeRefine 10) :: Amount 2
-- >>> amountDebit
-- Amount {amountSide = SideDebit, amountValue = Refined 10.00}
-- >>> let amountCredit = Amount SideCredit (unsafeRefine 10) :: Amount 2
-- >>> amountCredit
-- Amount {amountSide = SideCredit, amountValue = Refined 10.00}
-- >>> updateBalance balance amountDebit
-- Balance {balanceSide = SideDebit, balanceValue = 52.00, balanceInventory = MkInventory {inventoryTotal = 0.000000000000, inventoryCurrent = fromList [], inventoryHistory = fromList []}}
-- >>> updateBalance balance amountCredit
-- Balance {balanceSide = SideDebit, balanceValue = 32.00, balanceInventory = MkInventory {inventoryTotal = 0.000000000000, inventoryCurrent = fromList [], inventoryHistory = fromList []}}
updateBalance
  :: KnownNat precision
  => Balance precision
  -> Amount precision
  -> Balance precision
updateBalance (Balance bSide bVal inventory) (Amount aSide aVal) =
  Balance bSide (bVal + (unrefine aVal * (if bSide == aSide then 1 else (-1)))) inventory


-- | Updates the balance with additional inventory event.
updateBalanceWithInventory
  :: KnownNat precision
  => Day
  -> Balance precision
  -> Amount precision
  -> Quantity 12
  -> ([InventoryHistoryItem 8 12 precision], Balance precision)
updateBalanceWithInventory date balance amount quantity =
  let nb = updateBalance balance amount
      vv = abs (valueFromAmount AccountKindAsset amount)
      (is, ni) = updateInventoryVV date vv quantity (balanceInventory nb)
   in (Foldable.toList is, nb {balanceInventory = ni})


-- | Converts the balance to amount.
--
-- >>> :set -XDataKinds
-- >>> import Data.Default (def)
-- >>> import Haspara.Accounting.Side
-- >>> amountFromBalance (Balance SideDebit 42 def :: Balance 2)
-- Amount {amountSide = SideDebit, amountValue = Refined 42.00}
-- >>> amountFromBalance (Balance SideDebit (-42) def :: Balance 2)
-- Amount {amountSide = SideCredit, amountValue = Refined 42.00}
-- >>> amountFromBalance (Balance SideCredit 42 def :: Balance 2)
-- Amount {amountSide = SideCredit, amountValue = Refined 42.00}
-- >>> amountFromBalance (Balance SideCredit (-42) def :: Balance 2)
-- Amount {amountSide = SideDebit, amountValue = Refined 42.00}
amountFromBalance
  :: KnownNat precision
  => Balance precision
  -> Amount precision
amountFromBalance (Balance side value _) =
  Amount (if value < 0 then otherSide side else side) (absQuantity value)


-- | Returns the quantity of the balance given the account kind.
--
-- See 'quantityFromAmount' for the meaning of quantity.
quantityFromBalance
  :: KnownNat precision
  => AccountKind
  -> Balance precision
  -> Quantity precision
quantityFromBalance k = quantityFromAmount k . amountFromBalance


-- | Returns the value of the balance given the account kind.
--
-- See 'valueFromAmount' for the meaning of quantity.
valueFromBalance
  :: KnownNat precision
  => AccountKind
  -> Balance precision
  -> Quantity precision
valueFromBalance k = valueFromAmount k . amountFromBalance
