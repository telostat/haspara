{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Tests.Haspara.Accounting.Inventory where

import Data.Default (def)
import Data.Foldable (foldl')
import qualified Data.Sequence as Seq
import qualified Data.Time as Time
import Haspara.Accounting.Inventory (Inventory (..), InventoryEvent (..), InventoryHistoryItem (..), updateInventory, updateInventoryVP)
import Haspara.Quantity (Quantity)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)


-- * Core Definitions


-- | Type definition for the inventory type used in this module.
type TestInventory = Inventory 8 12 2


-- | Type definition for the inventory type used in this module.
type TestInventoryEvent = InventoryEvent 8 12


-- | Type definition for the inventory history item used in this module.
type TestInventoryHistoryItem = InventoryHistoryItem 8 12 2


-- | Initial inventory.
initInventory :: TestInventory
initInventory = def @TestInventory


testInventory :: IO ()
testInventory = hspec $ do
  describe "Inventory Functionality" $ do
    testInitialInventory
    testInitialInventoryEvent
    testInventoryCloseProfit
    testInventoryAlternate
    testInventoryScenario1


-- * Test Specs


testInitialInventory :: SpecWith ()
testInitialInventory =
  it "initial inventory is empty" $ do
    inventoryTotal initInventory `shouldBe` 0
    inventoryCurrent initInventory `shouldBe` mempty
    inventoryHistory initInventory `shouldBe` mempty


testInitialInventoryEvent :: SpecWith ()
testInitialInventoryEvent = do
  it "can add initial inventory increment event" $ do
    let (historyItem, newInventory) = updateInventoryVP (day 1) 1 10 initInventory

    inventoryTotal newInventory `shouldBe` 10
    length (inventoryCurrent newInventory) `shouldBe` 1
    inventoryHistory newInventory `shouldBe` mempty
    historyItem `shouldBe` mempty

  it "can add initial inventory decrement event" $ do
    let (historyItem, newInventory) = updateInventoryVP (day 1) 1 (-10) initInventory

    inventoryTotal newInventory `shouldBe` (-10)
    length (inventoryCurrent newInventory) `shouldBe` 1
    inventoryHistory newInventory `shouldBe` mempty
    historyItem `shouldBe` mempty


testInventoryCloseProfit :: SpecWith ()
testInventoryCloseProfit = do
  it "can close initial stock item with no-profit" $ do
    let (historyItem, newInventory) = updateManyVP [(day 1, 1, 10), (day 2, 1, -10)]

    inventoryTotal newInventory `shouldBe` 0
    inventoryCurrent newInventory `shouldBe` mempty
    length (inventoryHistory newInventory) `shouldBe` 1
    length historyItem `shouldBe` 1
    foldl' (\x y -> x + inventoryHistoryItemPnl y) 0 historyItem `shouldBe` 0

  it "can close initial stock item with profit" $ do
    let (historyItem, newInventory) = updateManyVP [(day 1, 1, 10), (day 2, 2, -10)]

    inventoryTotal newInventory `shouldBe` 0
    inventoryCurrent newInventory `shouldBe` mempty
    length (inventoryHistory newInventory) `shouldBe` 1
    length historyItem `shouldBe` 1
    foldl' (\x y -> x + inventoryHistoryItemPnl y) 0 historyItem `shouldBe` 10

  it "can close initial stock item with loss" $ do
    let (historyItem, newInventory) = updateManyVP [(day 1, 2, 10), (day 2, 1, -10)]

    inventoryTotal newInventory `shouldBe` 0
    inventoryCurrent newInventory `shouldBe` mempty
    length (inventoryHistory newInventory) `shouldBe` 1
    length historyItem `shouldBe` 1
    foldl' (\x y -> x + inventoryHistoryItemPnl y) 0 historyItem `shouldBe` -10


testInventoryAlternate :: SpecWith ()
testInventoryAlternate = do
  it "can switch from long to short" $ do
    let (historyItem, newInventory) = updateManyVP [(day 1, 1, 10), (day 2, 1, -15)]

    inventoryTotal newInventory `shouldBe` -5
    length (inventoryCurrent newInventory) `shouldBe` 1
    length (inventoryHistory newInventory) `shouldBe` 1
    length historyItem `shouldBe` 1
    foldl' (\x y -> x + inventoryHistoryItemPnl y) 0 historyItem `shouldBe` 0

  it "can switch from short to long" $ do
    let (historyItem, newInventory) = updateManyVP [(day 1, 1, -10), (day 2, 1, 15)]

    inventoryTotal newInventory `shouldBe` 5
    length (inventoryCurrent newInventory) `shouldBe` 1
    length (inventoryHistory newInventory) `shouldBe` 1
    length historyItem `shouldBe` 1
    foldl' (\x y -> x + inventoryHistoryItemPnl y) 0 historyItem `shouldBe` 0

  it "can alternate between long and short" $ do
    let (historyItem, newInventory) = updateManyVP [(day 1, 1, 10), (day 2, 1, -15), (day 3, 1, 20)]

    inventoryTotal newInventory `shouldBe` 15
    length (inventoryCurrent newInventory) `shouldBe` 1
    length (inventoryHistory newInventory) `shouldBe` 2
    length historyItem `shouldBe` 1
    foldl' (\x y -> x + inventoryHistoryItemPnl y) 0 historyItem `shouldBe` 0


testInventoryScenario1 :: SpecWith ()
testInventoryScenario1 = do
  it "can perform complex stuff" $ do
    let events =
          [ (day 1, 1, 10)
          , (day 2, 2, -15)
          , (day 3, 1, 0)
          , (day 4, 1, 5)
          , (day 5, 1, 0)
          , (day 6, 1, 5)
          , (day 7, 1, -5)
          , (day 8, 1, -15)
          , (day 9, 1, -10)
          , (day 10, 1, 25)
          , (day 11, 1, 0)
          , (day 12, 1, 15)
          , (day 13, 2, 10)
          , (day 14, 3, -10)
          , (day 15, 4, -15)
          ]

    let (historyItem, newInventory) = updateManyVP events

    inventoryTotal newInventory `shouldBe` 0
    inventoryTotal newInventory `shouldBe` foldl' (\a (_, _, x) -> a + x) 0 events
    length (inventoryCurrent newInventory) `shouldBe` 0
    length (inventoryHistory newInventory) `shouldBe` 8
    length historyItem `shouldBe` 2
    foldl' (\x y -> x + inventoryHistoryItemPnl y) 0 (inventoryHistory newInventory) `shouldBe` 70


-- * Helper Functions


day :: Integer -> Time.Day
day t = Time.addDays t (read "2020-01-01")


updateMany
  :: [TestInventoryEvent]
  -> (Seq.Seq TestInventoryHistoryItem, TestInventory)
updateMany = foldl' (\(_, s) i -> updateInventory i s) (mempty, initInventory)


updateManyVP
  :: [(Time.Day, Quantity 8, Quantity 12)]
  -> (Seq.Seq TestInventoryHistoryItem, TestInventory)
updateManyVP = updateMany . fmap (\(x, y, z) -> InventoryEvent x y z)
