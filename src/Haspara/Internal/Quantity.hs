{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Haspara.Internal.Quantity where

import           Control.Applicative        (liftA2)
import           Control.Monad.Except       (MonadError(throwError))
import qualified Data.Aeson                 as Aeson
import           Data.Either                (fromRight)
import           Data.Proxy                 (Proxy(..))
import qualified Data.Scientific            as S
import           GHC.Generics               (Generic)
import           GHC.TypeLits               (KnownNat, Nat, natVal, type (+))
import qualified Language.Haskell.TH.Syntax as TH
import qualified Numeric.Decimal            as D


-- $setup
-- >>> :set -XDataKinds


-- | Type encoding for common quantity values with given scaling (digits after
-- the decimal point).
--
-- >>> 42 :: Quantity 0
-- 42
-- >>> 42 :: Quantity 1
-- 42.0
-- >>> 42 :: Quantity 2
-- 42.00
-- >>> 41 + 1 :: Quantity 2
-- 42.00
-- >>> 43 - 1 :: Quantity 2
-- 42.00
-- >>> 2 * 3 * 7 :: Quantity 2
-- 42.00
-- >>> negate (-42) :: Quantity 2
-- 42.00
-- >>> abs (-42) :: Quantity 2
-- 42.00
-- >>> signum (-42) :: Quantity 2
-- -1.00
-- >>> fromInteger 42 :: Quantity 2
-- 42.00
-- >>> quantity 0.415 :: Quantity 2
-- 0.42
-- >>> quantity 0.425 :: Quantity 2
-- 0.42
-- >>> quantityLossless 0.42 :: Either String (Quantity 2)
-- Right 0.42
-- >>> quantityLossless 0.415 :: Either String (Quantity 2)
-- Left "Underflow while trying to create quantity: 0.415"
newtype Quantity (s :: Nat) = MkQuantity { unQuantity :: D.Decimal D.RoundHalfEven s Integer }
  deriving (Eq, Ord, Generic, Num)


-- | Orphan 'TH.Lift' instance for 'Quantity'.
--
-- TODO: Avoid having an orphan instance for @Decimal r s p@?
deriving instance TH.Lift (D.Decimal D.RoundHalfEven s Integer)


-- | 'TH.Lift' instance for 'Quantity'.
deriving instance TH.Lift (Quantity s)


-- | 'Aeson.FromJSON' instance for 'Quantity'.
--
-- >>> Aeson.decode "0.42" :: Maybe (Quantity 2)
-- Just 0.42
-- >>> Aeson.decode "0.415" :: Maybe (Quantity 2)
-- Just 0.42
-- >>> Aeson.decode "0.425" :: Maybe (Quantity 2)
-- Just 0.42
instance (KnownNat s) => Aeson.FromJSON (Quantity s) where
  parseJSON = Aeson.withScientific "Quantity" (pure . quantity)


-- | 'Aeson.ToJSON' instance for 'Quantity'.
--
-- >>> Aeson.encode (quantity 0.42 :: Quantity 2)
-- "0.42"
instance (KnownNat s) => Aeson.ToJSON (Quantity s) where
  toJSON = Aeson.Number . D.toScientificDecimal . unQuantity


-- | Numeric arithmetic over 'Quantity' values.
--
-- >>> import Numeric.Decimal
-- >>> let a = Arith (quantity 10) + Arith (quantity 32) :: Arith (Quantity 2)
-- >>> arithMaybe a
-- Just 42.00
-- >>> arithM (41 + 1) :: Either SomeException (Quantity 2)
-- Right 42.00
-- >>> arithM (43 - 1) :: Either SomeException (Quantity 2)
-- Right 42.00
-- >>> arithM (2 * 3 * 7) :: Either SomeException (Quantity 2)
-- Right 42.00
-- >>> arithM (signum 42) :: Either SomeException (Quantity 2)
-- Right 1.00
-- >>> arithM (signum (-42)) :: Either SomeException (Quantity 2)
-- Right -1.00
-- >>> arithM (abs 42) :: Either SomeException (Quantity 2)
-- Right 42.00
-- >>> arithM (abs (-42)) :: Either SomeException (Quantity 2)
-- Right 42.00
-- >>> arithM (fromInteger 42) :: Either SomeException (Quantity 2)
-- Right 42.00
instance (KnownNat s) => Num (D.Arith (Quantity s)) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  signum = fmap signum
  abs = fmap abs
  fromInteger = pure . MkQuantity . D.fromIntegerDecimal


-- | Fractional arithmetic over 'Quantity' values.
--
-- >>> import Numeric.Decimal
-- >>> arithM (fromRational 0.42) :: Either SomeException (Quantity 2)
-- Right 0.42
-- >>> arithM (fromRational 0.415) :: Either SomeException (Quantity 2)
-- Left PrecisionLoss (83 % 200) to 2 decimal spaces
-- >>> arithM $ (fromRational 0.84) / (fromRational 2) :: Either SomeException (Quantity 2)
-- Right 0.42
-- >>> arithM $ (fromRational 0.42) / (fromRational 0) :: Either SomeException (Quantity 2)
-- Left divide by zero
-- >>> let a = 84 :: Quantity 2
-- >>> let b =  2 :: Quantity 2
-- >>> let c =  0 :: Quantity 2
-- >>> arithM (Arith a / Arith b) :: Either SomeException (Quantity 2)
-- Right 42.00
-- >>> arithM (Arith a / Arith b / Arith c) :: Either SomeException (Quantity 2)
-- Left divide by zero
instance (KnownNat s) => Fractional (D.Arith (Quantity s)) where
  a / b = fmap MkQuantity $ fmap unQuantity a / fmap unQuantity b
  fromRational = fmap MkQuantity . D.fromRationalDecimalWithoutLoss


-- | 'Show' instance for 'Quantity'.
--
-- >>> show (42 :: Quantity 2)
-- "42.00"
-- >>> 42 :: Quantity 2
-- 42.00
instance KnownNat s => Show (Quantity s) where
  show = show . unQuantity


-- | Constructs 'Quantity' values from 'S.Scientific' values in a lossy way.
--
-- This function uses 'quantityAux' in case that the lossless attempt fails. We
-- could have used 'quantityAux' directly. However, 'quantityAux' is doing too
-- much (see 'roundScientific'). Therefore, we are first attempting a lossless
-- construction (see 'quantityLossless') and we fallback to 'quantityAux' in
-- case the lossless construction fails.
--
-- >>> quantity 0 :: Quantity 0
-- 0
-- >>> quantity 0 :: Quantity 1
-- 0.0
-- >>> quantity 0 :: Quantity 2
-- 0.00
-- >>> quantity 0.04 :: Quantity 1
-- 0.0
-- >>> quantity 0.05 :: Quantity 1
-- 0.0
-- >>> quantity 0.06 :: Quantity 1
-- 0.1
-- >>> quantity 0.14 :: Quantity 1
-- 0.1
-- >>> quantity 0.15 :: Quantity 1
-- 0.2
-- >>> quantity 0.16 :: Quantity 1
-- 0.2
-- >>> quantity 0.04 :: Quantity 2
-- 0.04
-- >>> quantity 0.05 :: Quantity 2
-- 0.05
-- >>> quantity 0.06 :: Quantity 2
-- 0.06
-- >>> quantity 0.14 :: Quantity 2
-- 0.14
-- >>> quantity 0.15 :: Quantity 2
-- 0.15
-- >>> quantity 0.16 :: Quantity 2
-- 0.16
-- >>> quantity 0.04 :: Quantity 3
-- 0.040
-- >>> quantity 0.05 :: Quantity 3
-- 0.050
-- >>> quantity 0.06 :: Quantity 3
-- 0.060
-- >>> quantity 0.14 :: Quantity 3
-- 0.140
-- >>> quantity 0.15 :: Quantity 3
-- 0.150
-- >>> quantity 0.16 :: Quantity 3
-- 0.160
quantity :: KnownNat s => S.Scientific -> Quantity s
quantity s = case quantityLossless s of
  Left _   -> quantityAux s
  Right dv -> dv


-- | Auxiliary function for 'quantity' implementation.
--
-- See 'quantity' why we need this function and why we haven't used it as the
-- direct implementation of 'quantity'.
--
-- Call-sites should avoid using this function directly due to its performance
-- characteristics.
quantityAux :: forall s. KnownNat s => S.Scientific -> Quantity s
quantityAux x = fromRight err $ quantityLossless (roundScientific nof x)
  where
    -- Get the term-level scaling for the target value:
    nof = fromIntegral $ natVal (Proxy :: Proxy s)

    -- This function should NOT fail in practice ever, but theoretically it can
    -- due to type signatures. We will let it error with a message to ourselves:
    err = error $ "PROGRAMMING ERROR: Can not construct 'Quantity " <> show nof <> "' with '" <> show x <> "' in a lossy way."


-- | Constructs 'Quantity' values from 'S.Scientific' values in a lossy way.
--
-- >>> quantityLossless 0 :: Either String (Quantity 0)
-- Right 0
-- >>> quantityLossless 0 :: Either String (Quantity 1)
-- Right 0.0
-- >>> quantityLossless 0 :: Either String (Quantity 2)
-- Right 0.00
-- >>> quantityLossless 0.04 :: Either String (Quantity 1)
-- Left "Underflow while trying to create quantity: 4.0e-2"
-- >>> quantityLossless 0.05 :: Either String (Quantity 1)
-- Left "Underflow while trying to create quantity: 5.0e-2"
-- >>> quantityLossless 0.06 :: Either String (Quantity 1)
-- Left "Underflow while trying to create quantity: 6.0e-2"
-- >>> quantityLossless 0.14 :: Either String (Quantity 1)
-- Left "Underflow while trying to create quantity: 0.14"
-- >>> quantityLossless 0.15 :: Either String (Quantity 1)
-- Left "Underflow while trying to create quantity: 0.15"
-- >>> quantityLossless 0.16 :: Either String (Quantity 1)
-- Left "Underflow while trying to create quantity: 0.16"
-- >>> quantityLossless 0.04 :: Either String (Quantity 2)
-- Right 0.04
-- >>> quantityLossless 0.05 :: Either String (Quantity 2)
-- Right 0.05
-- >>> quantityLossless 0.06 :: Either String (Quantity 2)
-- Right 0.06
-- >>> quantityLossless 0.14 :: Either String (Quantity 2)
-- Right 0.14
-- >>> quantityLossless 0.15 :: Either String (Quantity 2)
-- Right 0.15
-- >>> quantityLossless 0.16 :: Either String (Quantity 2)
-- Right 0.16
-- >>> quantityLossless 0.04 :: Either String (Quantity 3)
-- Right 0.040
-- >>> quantityLossless 0.05 :: Either String (Quantity 3)
-- Right 0.050
-- >>> quantityLossless 0.06 :: Either String (Quantity 3)
-- Right 0.060
-- >>> quantityLossless 0.14 :: Either String (Quantity 3)
-- Right 0.140
-- >>> quantityLossless 0.15 :: Either String (Quantity 3)
-- Right 0.150
-- >>> quantityLossless 0.16 :: Either String (Quantity 3)
-- Right 0.160
quantityLossless :: (KnownNat s, MonadError String m) => S.Scientific -> m (Quantity s)
quantityLossless s = either (const $ throwError ("Underflow while trying to create quantity: " <> show s)) (pure . MkQuantity) $ D.fromScientificDecimal s


-- | Rounds given quantity by @k@ digits.
--
-- >>> roundQuantity (quantity 0.415 :: Quantity 3) :: Quantity 2
-- 0.42
-- >>> roundQuantity (quantity 0.425 :: Quantity 3) :: Quantity 2
-- 0.42
roundQuantity :: KnownNat k => Quantity (n + k) -> Quantity n
roundQuantity (MkQuantity d) = MkQuantity (D.roundDecimal d)


-- | Multiplies two quantities with different scales and rounds back to the scale of the frst operand.
--
-- >>> times (quantity 0.42 :: Quantity 2) (quantity 0.42 :: Quantity 2)
-- 0.18
times :: (KnownNat s, KnownNat k) => Quantity s -> Quantity k -> Quantity s
times q1 q2 = roundQuantity (timesLossless q1 q2)


-- | Multiplies two quantities with different scales.
--
-- >>> timesLossless (quantity 0.42 :: Quantity 2) (quantity 0.42 :: Quantity 2)
-- 0.1764
timesLossless :: (KnownNat s, KnownNat k) => Quantity s -> Quantity k -> Quantity (s + k)
timesLossless (MkQuantity d1) (MkQuantity d2) = MkQuantity (D.timesDecimal d1 d2)


-- | Rounds a given scientific into a new scientific with given max digits after
-- decimal point.
--
-- This uses half-even rounding method.
--
-- >>> roundScientific 0 0.4
-- 0.0
-- >>> roundScientific 0 0.5
-- 0.0
-- >>> roundScientific 0 0.6
-- 1.0
-- >>> roundScientific 0 1.4
-- 1.0
-- >>> roundScientific 0 1.5
-- 2.0
-- >>> roundScientific 0 1.6
-- 2.0
-- >>> roundScientific 1 0.04
-- 0.0
-- >>> roundScientific 1 0.05
-- 0.0
-- >>> roundScientific 1 0.06
-- 0.1
-- >>> roundScientific 1 0.14
-- 0.1
-- >>> roundScientific 1 0.15
-- 0.2
-- >>> roundScientific 1 0.16
-- 0.2
-- >>> roundScientific 1 3.650
-- 3.6
-- >>> roundScientific 1 3.740
-- 3.7
-- >>> roundScientific 1 3.749
-- 3.7
-- >>> roundScientific 1 3.750
-- 3.8
-- >>> roundScientific 1 3.751
-- 3.8
-- >>> roundScientific 1  3.760
-- 3.8
-- >>> roundScientific 1 (-3.650)
-- -3.6
-- >>> roundScientific 1 (-3.740)
-- -3.7
-- >>> roundScientific 1 (-3.749)
-- -3.7
-- >>> roundScientific 1 (-3.750)
-- -3.8
-- >>> roundScientific 1 (-3.751)
-- -3.8
-- >>> roundScientific 1 (-3.760)
-- -3.8
--
-- TODO: Refactor to improve the performance of this function.
roundScientific :: Int -> S.Scientific -> S.Scientific
roundScientific = (read .) . S.formatScientific S.Fixed . Just
