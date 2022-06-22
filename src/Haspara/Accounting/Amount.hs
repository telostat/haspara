-- | This module provides definitions for amounts used as in accounting.
--
-- For balance definition that allows "Negative Balance" phenomenon, see
-- 'Haspara.Accounting.Balance'.

{-# LANGUAGE DataKinds #-}

module Haspara.Accounting.Amount where

import qualified Data.Aeson                 as Aeson
import           GHC.Generics               (Generic)
import           GHC.TypeLits               (KnownNat, Nat)
import           Haspara.Accounting.Account (AccountKind(..))
import           Haspara.Accounting.Side    (Side(..), sideByAccountKind)
import           Haspara.Internal.Aeson     (commonAesonOptions)
import           Haspara.Quantity           (Quantity, UnsignedQuantity, absQuantity)
import           Refined                    (unrefine)


-- | Data definition for amounts.
data Amount (precision :: Nat) = Amount
  { amountSide  :: !Side
  , amountValue :: !(UnsignedQuantity precision)
  }
  deriving (Eq, Generic, Ord, Show)


-- | 'Aeson.FromJSON' instance for 'Amount'.
--
-- >>> Aeson.eitherDecode "{\"side\": \"db\", \"value\": 42}" :: Either String (Amount 2)
-- Right (Amount {amountSide = SideDebit, amountValue = Refined 42.00})
-- >>> Aeson.eitherDecode "{\"side\": \"cr\", \"value\": 42}" :: Either String (Amount 2)
-- Right (Amount {amountSide = SideCredit, amountValue = Refined 42.00})
instance KnownNat precision => Aeson.FromJSON (Amount precision) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "amount"


-- | 'Aeson.ToJSON' instance for 'Amount'.
--
-- >>> import Haspara.Accounting.Side
-- >>> import Haspara.Quantity
-- >>> import Refined.Unsafe
-- >>> Aeson.encode (Amount SideDebit (unsafeRefine (mkQuantity 42 :: Quantity 2)))
-- "{\"side\":\"db\",\"value\":42.0}"
-- >>> Aeson.encode (Amount SideCredit (unsafeRefine (mkQuantity 42 :: Quantity 2)))
-- "{\"side\":\"cr\",\"value\":42.0}"
-- >>> Aeson.eitherDecode (Aeson.encode (Amount SideDebit (unsafeRefine (mkQuantity 42 :: Quantity 2)))) :: Either String (Amount 2)
-- Right (Amount {amountSide = SideDebit, amountValue = Refined 42.00})
-- >>> Aeson.eitherDecode (Aeson.encode (Amount SideCredit (unsafeRefine (mkQuantity 42 :: Quantity 2)))) :: Either String (Amount 2)
-- Right (Amount {amountSide = SideCredit, amountValue = Refined 42.00})
instance KnownNat precision => Aeson.ToJSON (Amount precision) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "amount"


-- | Returns the debit value of the 'Amount', if any.
amountDebit :: KnownNat precision => Amount precision -> Maybe (UnsignedQuantity precision)
amountDebit (Amount SideDebit value) = Just value
amountDebit _                        = Nothing


-- | Returns the credit value of the 'Amount', if any.
amountCredit :: KnownNat precision => Amount precision -> Maybe (UnsignedQuantity precision)
amountCredit (Amount SideCredit value) = Just value
amountCredit _                         = Nothing


-- | Builds the 'Amount' for the given /value/ for the given 'AccountKind'.
--
-- The /value/ concept here refers to the value of a particular economic event
-- as in the contribution of that event to the net-worth of the entity.
--
-- This definition of the value is different than what we refer to in
-- 'amountFromQuantity'. In 'amountFromQuantity' the /quantity/ is simply
-- reflecting the increment or decrement in a particular account of a particular
-- 'AccountKind'.
--
-- For example, consider getting a loan: There are two immediate events due to
-- this exchange:
--
-- 1. Inflow of cash of some quantity to an 'AccountKindAsset' account.
-- 2. Inflow of loan contract with some notional value of the same quantity to a
--    'AccountKindLiability acount.
--
-- Let's say, the notional is USD 1,000. Therefore:
--
-- 1. Inflow of USD 1,000 to the cash account.
-- 2. Inflow of a Loan Contract of USD 1,000 to the liability account.
--
-- Conventionally, the latter is reflected as follow:
--
-- >>> import Haspara.Quantity
-- >>> amountFromQuantity AccountKindLiability (mkQuantity 1000 :: Quantity 2)
-- Amount {amountSide = SideCredit, amountValue = Refined 1000.00}
--
-- However, if the call-site is referring to values as in the net effect of the
-- event to the net-worth of the entity, then:
--
-- >>> amountFromValue AccountKindLiability (mkQuantity (-1000) :: Quantity 2)
-- Amount {amountSide = SideCredit, amountValue = Refined 1000.00}
--
-- For reference, given:
--
-- >>> let valPos = mkQuantity 42 :: Quantity 2
-- >>> let valNeg = mkQuantity (-42) :: Quantity 2
--
-- ..., let's consider following events:
--
-- We have an inflow and outflow of some assets, respectively:
--
-- >>> amountFromValue AccountKindAsset valPos
-- Amount {amountSide = SideDebit, amountValue = Refined 42.00}
-- >>> amountFromValue AccountKindAsset valNeg
-- Amount {amountSide = SideCredit, amountValue = Refined 42.00}
--
-- We have some decrease and increase in our liabilities, respectively:
--
-- >>> amountFromValue AccountKindLiability valPos
-- Amount {amountSide = SideDebit, amountValue = Refined 42.00}
-- >>> amountFromValue AccountKindLiability valNeg
-- Amount {amountSide = SideCredit, amountValue = Refined 42.00}
--
-- We have some increase and decrease in our equity, respectively:
--
-- >>> amountFromValue AccountKindEquity valPos
-- Amount {amountSide = SideCredit, amountValue = Refined 42.00}
-- >>> amountFromValue AccountKindEquity valNeg
-- Amount {amountSide = SideDebit, amountValue = Refined 42.00}
--
-- We have some profit and loss in our PnL, respectively:
--
-- >>> amountFromValue AccountKindRevenue valPos
-- Amount {amountSide = SideCredit, amountValue = Refined 42.00}
-- >>> amountFromValue AccountKindRevenue valNeg
-- Amount {amountSide = SideDebit, amountValue = Refined 42.00}
--
-- We have some decrease and increase in our expenses, respectively:
--
-- >>> amountFromValue AccountKindExpense valPos
-- Amount {amountSide = SideCredit, amountValue = Refined 42.00}
-- >>> amountFromValue AccountKindExpense valNeg
-- Amount {amountSide = SideDebit, amountValue = Refined 42.00}
amountFromValue
  :: KnownNat precision
  => AccountKind
  -> Quantity precision
  -> Amount precision
amountFromValue k q = case k of
  AccountKindAsset     -> Amount { amountSide = if q >= 0 then SideDebit else SideCredit, amountValue = absQuantity q }
  AccountKindLiability -> Amount { amountSide = if q >= 0 then SideDebit else SideCredit, amountValue = absQuantity q }
  AccountKindEquity    -> Amount { amountSide = if q >= 0 then SideCredit else SideDebit, amountValue = absQuantity q }
  AccountKindRevenue   -> Amount { amountSide = if q >= 0 then SideCredit else SideDebit, amountValue = absQuantity q }
  AccountKindExpense   -> Amount { amountSide = if q >= 0 then SideCredit else SideDebit, amountValue = absQuantity q }


-- | Returns the 'Value' for the given 'Amount' for the given 'AccountKind'.
--
-- This is dual to 'amountFromValue'.
--
-- For values of positive and negative net-effect on the net-worth of the
-- entity, respectively:
--
-- >>> import Haspara.Quantity
-- >>> let valPos = mkQuantity 42 :: Quantity 2
-- >>> let valNeg = mkQuantity (-42) :: Quantity 2
--
-- ..., for a @check@ function that checks if the roundtrip to a value is
-- successful for a given 'AccountKind':
--
-- >>> let check = \k v -> v == valueFromAmount k (amountFromValue k v)
--
-- ..., and for the list of 'AccountKind's.
--
-- >>> let kinds = [minBound .. maxBound] :: [AccountKind]
-- >>> kinds
-- [AccountKindAsset,AccountKindLiability,AccountKindEquity,AccountKindRevenue,AccountKindExpense]
--
-- All checks should pass:
--
-- >>> all (\k -> check k valPos && check k valNeg) kinds
-- True
valueFromAmount
  :: KnownNat precision
  => AccountKind
  -> Amount precision
  -> Quantity precision
valueFromAmount k (Amount s v) = case (k, s, unrefine v) of
  (AccountKindAsset, SideDebit, q)      -> q
  (AccountKindAsset, SideCredit, q)     -> -q
  (AccountKindLiability, SideDebit, q)  -> q
  (AccountKindLiability, SideCredit, q) -> -q
  (AccountKindEquity, SideDebit, q)     -> -q
  (AccountKindEquity, SideCredit, q)    -> q
  (AccountKindRevenue, SideDebit, q)    -> -q
  (AccountKindRevenue, SideCredit, q)   -> q
  (AccountKindExpense, SideDebit, q)    -> -q
  (AccountKindExpense, SideCredit, q)   -> q


-- | Builds the 'Amount' value for the given account kind and quantity.
--
-- The concept of /quantity/ here refers to the conventional concept of what it
-- means for an 'Account' of a given 'AccountKind'.
--
-- For example, a loan of USD 1,000 has an increase in our liabilities.
-- Therefore, the quantity is expected to be positive:
--
-- >>> import Haspara.Quantity
-- >>> amountFromQuantity AccountKindLiability (mkQuantity 1000 :: Quantity 2)
-- Amount {amountSide = SideCredit, amountValue = Refined 1000.00}
--
-- Note 'amountFromValue' function if you are rather working with values that
-- are conceptually different than the /quantity/ here whereby a /value/ refers
-- to the value of a particular economic event as in the contribution of that
-- event to the net-worth of the entity. Therefore, above example would be
-- reflected as follows to get the same 'Amount' value:
--
-- >>> amountFromValue AccountKindLiability (mkQuantity (-1000) :: Quantity 2)
-- Amount {amountSide = SideCredit, amountValue = Refined 1000.00}
--
-- Check 'amountFromValue' documentation for further information.
amountFromQuantity
  :: KnownNat precision
  => AccountKind
  -> Quantity precision
  -> Amount precision
amountFromQuantity k q =
  Amount
    { amountSide = sideByAccountKind k q
    , amountValue = absQuantity q
    }


-- | Returns the quantity for the given amount.
--
-- This is dual to 'amountFromQuantity'.
quantityFromAmount
  :: KnownNat precision
  => AccountKind
  -> Amount precision
  -> Quantity precision
quantityFromAmount k (Amount side absValue) =
  let
    value = unrefine absValue
  in
    case (k, side) of
      (AccountKindAsset, SideDebit)      -> value
      (AccountKindAsset, SideCredit)     -> -value
      (AccountKindLiability, SideDebit)  -> -value
      (AccountKindLiability, SideCredit) -> value
      (AccountKindEquity, SideDebit)     -> -value
      (AccountKindEquity, SideCredit)    -> value
      (AccountKindRevenue, SideDebit)    -> -value
      (AccountKindRevenue, SideCredit)   -> value
      (AccountKindExpense, SideDebit)    -> value
      (AccountKindExpense, SideCredit)   -> -value
