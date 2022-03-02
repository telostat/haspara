{-# LANGUAGE DataKinds #-}

module Haspara.Accounting.Event where

import           Control.Monad.Except     (MonadError(throwError))
import           Data.Aeson               ((.:), (.=))
import qualified Data.Aeson               as Aeson
import qualified Data.Char                as C
import qualified Data.Text                as T
import           Data.Time                (Day)
import           GHC.TypeLits             (KnownNat, Nat)
import qualified Haspara                  as H
import           Haspara.Accounting.Types (UnsignedQuantity)
import           Refined                  (refine)


-- | Encoding of an increment/decrement event.
--
-- >>> :set -XDataKinds
-- >>> import Refined
-- >>> let date = read "2021-01-01"
-- >>> let oid = 1 :: Int
-- >>> let qty = $$(refineTH 42) :: UnsignedQuantity 2
-- >>> let event = EventDecrement date oid qty
-- >>> let json = Aeson.encode event
-- >>> json
-- "{\"qty\":42.0,\"type\":\"DECREMENT\",\"obj\":1,\"date\":\"2021-01-01\"}"
-- >>> Aeson.decode json :: Maybe (Event Int 2)
-- Just (EventDecrement 2021-01-01 1 (Refined 42.00))
-- >>> Aeson.decode json == Just event
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


eventDate :: (KnownNat s) => Event o s -> Day
eventDate (EventDecrement d _ _) = d
eventDate (EventIncrement d _ _) = d


eventObject :: (KnownNat s) => Event o s -> o
eventObject (EventDecrement _ o _) = o
eventObject (EventIncrement _ o _) = o


negateEvent :: (KnownNat s) => Event o s -> Event o s
negateEvent (EventDecrement d o x) = EventIncrement d o x
negateEvent (EventIncrement d o x) = EventDecrement d o x


mkEvent :: (MonadError String m, KnownNat s) => Day -> o -> H.Quantity s -> m (Event o s)
mkEvent d o x
  | x < 0     = either (throwError . show) pure $ EventDecrement d o <$> refine (abs x)
  | otherwise = either (throwError . show) pure $ EventIncrement d o <$> refine (abs x)
