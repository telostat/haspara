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


-- | Builds the 'Amount' value for the given account kind and quantity.
amountFromQuantityWithSide
  :: KnownNat precision
  => AccountKind
  -> Quantity precision
  -> Amount precision
amountFromQuantityWithSide k q =
  Amount
    { amountSide = sideByAccountKind k q
    , amountValue = absQuantity q
    }


-- | Returns the nominal value of the amount given the account kind.
quantityFromAmountWithAccountKind
  :: KnownNat precision
  => AccountKind
  -> Amount precision
  -> Quantity precision
quantityFromAmountWithAccountKind k (Amount side absValue) =
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
