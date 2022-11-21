{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides FIFO machinery for inventory accounting.
module Haspara.Accounting.Inventory where

import qualified Data.Aeson as Aeson
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Time (Day)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat)
import Haspara.Internal.Aeson (commonAesonOptions)
import Haspara.Quantity (Quantity, divideD, times)


-- * Data Definitions


-- | Data definition that keeps track of inventory for an economic resource.
--
-- This data definition is polymorphic over the precision for, respectively:
--
-- 1. @pprec@ precision of the price values,
-- 2. @sprec@ precision of the inventory event quantities, and
-- 3. @vprec@ precision of the monetary values such as PnL.
--
-- Values of this type should not be directly manipulated. Instead, `def` is to
-- be used for initializing an empty inventory and `updateInventory` method (and
-- convenience wrappers) should be used to update the inventory with new
-- inventory events.
data Inventory (pprec :: Nat) (sprec :: Nat) (vprec :: Nat) = MkInventory
  { inventoryTotal :: !(Quantity sprec)
  , inventoryCurrent :: !(Seq.Seq (InventoryEvent pprec sprec))
  , inventoryHistory :: !(Seq.Seq (InventoryHistoryItem pprec sprec vprec))
  }
  deriving (Eq, Generic, Show)


instance (KnownNat pprec, KnownNat sprec, KnownNat vprec) => Default (Inventory pprec sprec vprec) where
  def =
    MkInventory
      { inventoryTotal = 0
      , inventoryCurrent = mempty
      , inventoryHistory = mempty
      }


instance (KnownNat pprec, KnownNat sprec, KnownNat vprec) => Aeson.FromJSON (Inventory pprec sprec vprec) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "inventory"


instance (KnownNat pprec, KnownNat sprec, KnownNat vprec) => Aeson.ToJSON (Inventory pprec sprec vprec) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "inventory"
  toEncoding = Aeson.genericToEncoding $ commonAesonOptions "inventory"


-- | Data definition for inventory events.
--
-- This data definition is polymorphic over the precision for, respectively:
--
-- 1. @pprec@ precision of the price values, and
-- 2. @sprec@ precision of the inventory event quantities.
data InventoryEvent (pprec :: Nat) (sprec :: Nat) = InventoryEvent
  { inventoryEventDate :: !Day
  , inventoryEventPrice :: !(Quantity pprec)
  , inventoryEventQuantity :: !(Quantity sprec)
  }
  deriving (Eq, Generic, Show)


instance (KnownNat pprec, KnownNat sprec) => Aeson.FromJSON (InventoryEvent pprec sprec) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "inventoryEvent"


instance (KnownNat pprec, KnownNat sprec) => Aeson.ToJSON (InventoryEvent pprec sprec) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "inventoryEvent"
  toEncoding = Aeson.genericToEncoding $ commonAesonOptions "inventoryEvent"


-- | Data definition for PnL-taking inventory history items.
--
-- Essentially, values of this type represent a pnl-taking for a long/short
-- inventory event and a matching short/long inventory event of the same
-- quantity. Date refers to the date of the later event. If prices are
-- different, PnL is non-zero.
--
-- This data definition is polymorphic over the precision for, respectively:
--
-- 1. @pprec@ precision of the price values,
-- 2. @sprec@ precision of the inventory event quantities, and
-- 3. @vprec@ precision of the monetary values such as PnL.
--
-- Values of this type should not be directly manipulated. `updateInventory`
-- method (and convenience wrappers) are in charge of creating values of this
-- data type.
data InventoryHistoryItem (pprec :: Nat) (sprec :: Nat) (vprec :: Nat) = MkInventoryHistoryItem
  { inventoryHistoryItemDate :: !Day
  , inventoryHistoryItemPnl :: !(Quantity vprec)
  , inventoryHistoryItemFst :: !(InventoryEvent pprec sprec)
  , inventoryHistoryItemSnd :: !(InventoryEvent pprec sprec)
  }
  deriving (Eq, Generic, Show)


instance (KnownNat pprec, KnownNat sprec, KnownNat vprec) => Aeson.FromJSON (InventoryHistoryItem pprec sprec vprec) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "inventoryHistoryItem"


instance (KnownNat pprec, KnownNat sprec, KnownNat vprec) => Aeson.ToJSON (InventoryHistoryItem pprec sprec vprec) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "inventoryHistoryItem"
  toEncoding = Aeson.genericToEncoding $ commonAesonOptions "inventoryHistoryItem"


-- * Operations


-- | Processes a new inventory event onto the inventory.
--
-- Any event with @0@ quantity is disregarded.
updateInventory
  :: KnownNat pprec
  => KnownNat sprec
  => KnownNat vprec
  => InventoryEvent pprec sprec
  -> Inventory pprec sprec vprec
  -> (Seq.Seq (InventoryHistoryItem pprec sprec vprec), Inventory pprec sprec vprec)
updateInventory event inventory = case inventoryEventQuantity event of
  0 -> (mempty, inventory)
  _ -> updateInventoryAux mempty event inventory


-- | Convenience wrapper for 'updateInventory' which works with date, price and
-- quantity.
updateInventoryVP
  :: KnownNat pprec
  => KnownNat sprec
  => KnownNat vprec
  => Day
  -> Quantity pprec
  -> Quantity sprec
  -> Inventory pprec sprec vprec
  -> (Seq.Seq (InventoryHistoryItem pprec sprec vprec), Inventory pprec sprec vprec)
updateInventoryVP date price quantity =
  updateInventory $
    InventoryEvent
      { inventoryEventDate = date
      , inventoryEventPrice = price
      , inventoryEventQuantity = quantity
      }


-- | Convenience wrapper for 'updateInventory' which works with date, price and
-- quantity.
updateInventoryVV
  :: KnownNat pprec
  => KnownNat sprec
  => KnownNat vprec
  => Day
  -> Quantity vprec
  -> Quantity sprec
  -> Inventory pprec sprec vprec
  -> (Seq.Seq (InventoryHistoryItem pprec sprec vprec), Inventory pprec sprec vprec)
updateInventoryVV date value quantity =
  let price = fromMaybe 0 $ value `divideD` quantity
   in updateInventoryVP date (abs price) quantity


-- * Internal Definitions


-- | Work-horse function for updating inventory.
--
-- This is where the whole trick happens in this module.
updateInventoryAux
  :: KnownNat pprec
  => KnownNat sprec
  => KnownNat vprec
  => Seq.Seq (InventoryHistoryItem pprec sprec vprec)
  -> InventoryEvent pprec sprec
  -> Inventory pprec sprec vprec
  -> (Seq.Seq (InventoryHistoryItem pprec sprec vprec), Inventory pprec sprec vprec)
updateInventoryAux history event inventory@MkInventory {..} =
  case Seq.viewl inventoryCurrent of
    Seq.EmptyL -> (history, addInventoryEvent event inventory)
    i Seq.:< is -> case whatMunch event i of
      MunchNo -> (history, addInventoryEvent event inventory)
      MunchAll ->
        let (historyItem, newInventory) = munchAll event i is inventory
         in (history Seq.|> historyItem, newInventory)
      MunchLeft quan ->
        let (newEvent, remEvent) = splitEvent quan event
            (historyItem, newInventory) = munchAll newEvent i is inventory
         in updateInventoryAux (history Seq.|> historyItem) remEvent newInventory
      MunchRight quan ->
        let (newEvent, remEvent) = splitEvent quan i
            (historyItem, newInventory) = munchAll event newEvent (remEvent Seq.<| is) inventory
         in (history Seq.|> historyItem, newInventory)


-- | Splits the event into two events as per the given quantity.
--
-- If the event has a quantity of @100@ and the desired quantity is @10@, this
-- function spits out two event with same information except that the first has
-- a quantity of @10@ and the second has a quantity of @90@.
splitEvent
  :: KnownNat pprec
  => KnownNat sprec
  => Quantity sprec
  -> InventoryEvent pprec sprec
  -> (InventoryEvent pprec sprec, InventoryEvent pprec sprec)
splitEvent qty event@InventoryEvent {..} =
  let newItemQty = (-qty)
      remItemQty = inventoryEventQuantity + qty

      newItem = event {inventoryEventQuantity = newItemQty}
      remItem = event {inventoryEventQuantity = remItemQty}
   in (newItem, remItem)


-- | Pushes a new inventory event to the inventory.
--
-- This function is to be called by the internal machinery that handles most of
-- the critical tasks. Direct calls to this function will bypass the entire
-- machinery.
addInventoryEvent
  :: KnownNat pprec
  => KnownNat sprec
  => KnownNat vprec
  => InventoryEvent pprec sprec
  -> Inventory pprec sprec vprec
  -> Inventory pprec sprec vprec
addInventoryEvent event@InventoryEvent {..} inventory@MkInventory {..} =
  inventory
    { inventoryTotal = inventoryTotal + inventoryEventQuantity
    , inventoryCurrent = inventoryCurrent Seq.|> event
    }


-- | Captures two events of same absolute quantities with different directions
-- into a profit-taking inventory history item and updates the inventory.
munchAll
  :: KnownNat pprec
  => KnownNat sprec
  => KnownNat vprec
  => InventoryEvent pprec sprec
  -> InventoryEvent pprec sprec
  -> Seq.Seq (InventoryEvent pprec sprec)
  -> Inventory pprec sprec vprec
  -> (InventoryHistoryItem pprec sprec vprec, Inventory pprec sprec vprec)
munchAll lEvent rEvent remainingEvents inventory@MkInventory {..} =
  let lValue = inventoryEventQuantity lEvent `times` inventoryEventPrice lEvent
      rValue = inventoryEventQuantity rEvent `times` inventoryEventPrice rEvent

      historyItem =
        MkInventoryHistoryItem
          { inventoryHistoryItemDate = inventoryEventDate lEvent
          , inventoryHistoryItemPnl = (-1) `times` (rValue + lValue)
          , inventoryHistoryItemFst = rEvent
          , inventoryHistoryItemSnd = lEvent
          }

      newInventory =
        inventory
          { inventoryTotal = inventoryTotal + inventoryEventQuantity lEvent
          , inventoryCurrent = remainingEvents
          , inventoryHistory = inventoryHistory Seq.|> historyItem
          }
   in (historyItem, newInventory)


-- | Data definition representing how two inventory events should be processed.
data Munch (sprec :: Nat)
  = MunchNo
  | MunchAll
  | MunchLeft (Quantity sprec)
  | MunchRight (Quantity sprec)
  deriving (Eq, Show)


-- | Figures out how two inventory events should be processed.
whatMunch
  :: KnownNat pprec
  => KnownNat sprec
  => InventoryEvent pprec sprec
  -> InventoryEvent pprec sprec
  -> Munch sprec
whatMunch l r
  | lsgn == rsgn = MunchNo
  | labs == rabs = MunchAll
  | labs > rabs = MunchLeft rqty
  | otherwise = MunchRight lqty
  where
    lqty = inventoryEventQuantity l
    labs = abs lqty
    lsgn = signum lqty

    rqty = inventoryEventQuantity r
    rabs = abs rqty
    rsgn = signum rqty
