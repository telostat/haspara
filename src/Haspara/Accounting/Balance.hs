-- | This module provides definitions for balances used as in accounting.

{-# LANGUAGE DataKinds #-}

module Haspara.Accounting.Balance where

import qualified Data.Aeson                 as Aeson
import           GHC.Generics               (Generic)
import           GHC.TypeLits               (KnownNat, Nat)
import           Haspara.Accounting.Account (AccountKind)
import           Haspara.Accounting.Amount  (Amount(Amount), quantityFromAmountWithAccountKind)
import           Haspara.Accounting.Side    (Side, otherSide)
import           Haspara.Internal.Aeson     (commonAesonOptions)
import           Haspara.Quantity           (Quantity, absQuantity)
import           Refined                    (unrefine)


-- | Data definition for balances.
--
-- This definition is similar to 'Haspara.Accounting.Amount.Amount', however,
-- the value is allowed to be negative to reflect "Negative Balance" phenomenon.
--
-- See https://www.accountingtools.com/articles/what-is-a-negative-balance.html
data Balance (precision :: Nat) = Balance
  { balanceSide  :: !Side
  , balanceValue :: !(Quantity precision)
  }
  deriving (Eq, Generic, Show)


-- | 'Aeson.FromJSON' instance for 'Balance'.
--
-- For normal balances:
--
-- >>> Aeson.eitherDecode "{\"side\": \"db\", \"value\": 42}" :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideDebit, balanceValue = 42.00})
-- >>> Aeson.eitherDecode "{\"side\": \"cr\", \"value\": 42}" :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideCredit, balanceValue = 42.00})
--
-- For negative balances:
--
-- >>> Aeson.eitherDecode "{\"side\": \"db\", \"value\": -42}" :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideDebit, balanceValue = -42.00})
-- >>> Aeson.eitherDecode "{\"side\": \"cr\", \"value\": -42}" :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideCredit, balanceValue = -42.00})
instance KnownNat precision => Aeson.FromJSON (Balance precision) where
  parseJSON = Aeson.genericParseJSON $ commonAesonOptions "balance"


-- | 'Aeson.ToJSON' instance for 'Balance'.
--
-- For normal balances:
--
-- >>> import Haspara.Accounting.Side
-- >>> import Haspara.Quantity
-- >>> Aeson.encode (Balance SideDebit (mkQuantity 42 :: Quantity 2))
-- "{\"side\":\"db\",\"value\":42.0}"
-- >>> Aeson.encode (Balance SideCredit (mkQuantity 42 :: Quantity 2))
-- "{\"side\":\"cr\",\"value\":42.0}"
-- >>> Aeson.eitherDecode (Aeson.encode (Balance SideDebit (mkQuantity 42 :: Quantity 2))) :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideDebit, balanceValue = 42.00})
-- >>> Aeson.eitherDecode (Aeson.encode (Balance SideCredit (mkQuantity 42 :: Quantity 2))) :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideCredit, balanceValue = 42.00})
--
-- For negative balances:
--
-- >>> Aeson.encode (Balance SideDebit (mkQuantity (-42) :: Quantity 2))
-- "{\"side\":\"db\",\"value\":-42.0}"
-- >>> Aeson.encode (Balance SideCredit (mkQuantity (-42) :: Quantity 2))
-- "{\"side\":\"cr\",\"value\":-42.0}"
-- >>> Aeson.eitherDecode (Aeson.encode (Balance SideDebit (mkQuantity (-42) :: Quantity 2))) :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideDebit, balanceValue = -42.00})
-- >>> Aeson.eitherDecode (Aeson.encode (Balance SideCredit (mkQuantity (-42) :: Quantity 2))) :: Either String (Balance 2)
-- Right (Balance {balanceSide = SideCredit, balanceValue = -42.00})
instance KnownNat precision => Aeson.ToJSON (Balance precision) where
  toJSON = Aeson.genericToJSON $ commonAesonOptions "balance"


-- | Updates the balance with the given amount.
--
-- >>> import Haspara.Accounting.Amount
-- >>> import Haspara.Accounting.Side
-- >>> import Refined.Unsafe
-- >>> let balance = Balance SideDebit 42 :: Balance 2
-- >>> balance
-- Balance {balanceSide = SideDebit, balanceValue = 42.00}
-- >>> let amountDebit = Amount SideDebit (unsafeRefine 10) :: Amount 2
-- >>> amountDebit
-- Amount {amountSide = SideDebit, amountValue = Refined 10.00}
-- >>> let amountCredit = Amount SideCredit (unsafeRefine 10) :: Amount 2
-- >>> amountCredit
-- Amount {amountSide = SideCredit, amountValue = Refined 10.00}
-- >>> updateBalance balance amountDebit
-- Balance {balanceSide = SideDebit, balanceValue = 52.00}
-- >>> updateBalance balance amountCredit
-- Balance {balanceSide = SideDebit, balanceValue = 32.00}
updateBalance
  :: KnownNat precision
  => Balance precision
  -> Amount precision
  -> Balance precision
updateBalance (Balance bSide bVal) (Amount aSide aVal) =
  Balance bSide (bVal + (unrefine aVal * (if bSide == aSide then 1 else (-1))))


-- | Converts the balance to amount.
--
-- >>> import Haspara.Accounting.Side
-- >>> toAmount (Balance SideDebit 42 :: Balance 2)
-- Amount {amountSide = SideDebit, amountValue = Refined 42.00}
-- >>> toAmount (Balance SideDebit (-42) :: Balance 2)
-- Amount {amountSide = SideCredit, amountValue = Refined 42.00}
-- >>> toAmount (Balance SideCredit 42 :: Balance 2)
-- Amount {amountSide = SideCredit, amountValue = Refined 42.00}
-- >>> toAmount (Balance SideCredit (-42) :: Balance 2)
-- Amount {amountSide = SideDebit, amountValue = Refined 42.00}
toAmount
  :: KnownNat precision
  => Balance precision
  -> Amount precision
toAmount (Balance side value) =
  Amount (if value < 0 then otherSide side else side) (absQuantity value)


-- | Returns the nominal value of the balance given the account kind.
toQuantityWithKind
  :: KnownNat precision
  => AccountKind
  -> Balance precision
  -> Quantity precision
toQuantityWithKind k = quantityFromAmountWithAccountKind k . toAmount
