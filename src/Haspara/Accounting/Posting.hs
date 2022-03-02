{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Haspara.Accounting.Posting where

import qualified Data.List.NonEmpty         as NE
import           Deriving.Aeson             (CustomJSON(CustomJSON), FromJSON, Generic, ToJSON)
import           Deriving.Aeson.Stock       (Vanilla)
import           GHC.TypeLits               (KnownNat, Nat)
import           Haspara.Accounting.Account (Account(accountKind))
import           Haspara.Accounting.Entry   (Entry, buildEntry)
import           Haspara.Accounting.Event   (Event, eventObject)


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


postingEvents :: (KnownNat s) => Posting a o s -> [o]
postingEvents (Posting es)  = eventObject . fst <$> NE.toList es


post :: (KnownNat s) => Posting a o s -> [(Account a, Entry o s)]
post (Posting xs)       = go (NE.toList xs)
  where
    go []              = []
    go ((ev, ac) : ys) = (ac, buildEntry ev (accountKind ac)) : go ys
