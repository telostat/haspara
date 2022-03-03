-- | This module provides definitions for economic events.
--
-- /Note: The concept is not YET REA-compatible although we want to achieve it
-- at some point/.

{-# LANGUAGE DataKinds #-}

module Haspara.Accounting.Event where

import           Control.Monad.Except (MonadError(throwError))
import           Data.Aeson           ((.:), (.=))
import qualified Data.Aeson           as Aeson
import qualified Data.Char            as C
import qualified Data.Text            as T
import           Data.Time            (Day)
import           GHC.TypeLits         (KnownNat, Nat)
import           Haspara.Quantity     (Quantity, UnsignedQuantity)
import           Refined              (refine)


-- | Type encoding of an economic increment/decrement event.
--
-- The event explicitly carries the date and quantity information along with a
-- parameterized, arbitrary object providing the source of the event.
--
-- >>> :set -XDataKinds
-- >>> let date = read "2021-01-01"
-- >>> let oid = 1 :: Int
-- >>> let qty = $$(Refined.refineTH 42) :: UnsignedQuantity 2
-- >>> let event = EventDecrement date oid qty
-- >>> let json = Data.Aeson.encode event
-- >>> json
-- "{\"qty\":42.0,\"type\":\"DECREMENT\",\"obj\":1,\"date\":\"2021-01-01\"}"
-- >>> Data.Aeson.decode @(Event Int 2) json
-- Just (EventDecrement 2021-01-01 1 (Refined 42.00))
-- >>> Data.Aeson.decode json == Just event
-- True
data Event o (s :: Nat) =
    EventDecrement Day o (UnsignedQuantity s)
  | EventIncrement Day o (UnsignedQuantity s)
  deriving (Eq, Ord, Show)


instance (Aeson.FromJSON o, KnownNat s) => Aeson.FromJSON (Event o s) where
  parseJSON = Aeson.withObject "Event" $ \o -> do
    dorc <- o .: "type"
    cons <- case T.map C.toUpper dorc of
      "DECREMENT" -> pure EventDecrement
      "INCREMENT" -> pure EventIncrement
      x           -> fail ("Unknown event type: " <> T.unpack x)
    date <- o .: "date"
    obj <- o .: "obj"
    qty <- o .: "qty"
    pure (cons date obj qty)


instance (Aeson.ToJSON o, KnownNat s) => Aeson.ToJSON (Event o s) where
  toJSON x = case x of
    EventDecrement d o q -> Aeson.object ["type" .= ("DECREMENT" :: T.Text), "date" .= d, "obj" .= o, "qty" .= q]
    EventIncrement d o q -> Aeson.object ["type" .= ("INCREMENT" :: T.Text), "date" .= d, "obj" .= o, "qty" .= q]


-- | Returns the date of the event.
eventDate :: (KnownNat s) => Event o s -> Day
eventDate (EventDecrement d _ _) = d
eventDate (EventIncrement d _ _) = d


-- | Returns the source object of the event.
eventObject :: (KnownNat s) => Event o s -> o
eventObject (EventDecrement _ o _) = o
eventObject (EventIncrement _ o _) = o


-- | Negates the event.
negateEvent :: (KnownNat s) => Event o s -> Event o s
negateEvent (EventDecrement d o x) = EventIncrement d o x
negateEvent (EventIncrement d o x) = EventDecrement d o x


-- | Smart constuctor for 'Event' values.
mkEvent
  :: MonadError String m
  => KnownNat s
  => Day         -- ^ Date of the event.
  -> o           -- ^ Source object of the event.
  -> Quantity s  -- ^ Quantity of the event.
  -> m (Event o s)
mkEvent d o x
  | x < 0     = either (throwError . show) pure $ EventDecrement d o <$> refine (abs x)
  | otherwise = either (throwError . show) pure $ EventIncrement d o <$> refine (abs x)
