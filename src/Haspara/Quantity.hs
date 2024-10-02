{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides definitions for modeling and working with quantities
-- with fixed decimal points.
module Haspara.Quantity where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson.Encoding
import Data.Either (fromRight)
import Data.Proxy (Proxy (..))
import Data.Scientific (FPFormat (Fixed), Scientific, formatScientific)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal, type (+))
import qualified Language.Haskell.TH.Syntax as TH
import qualified Numeric.Decimal as D
import Refined (NonNegative, Refined, unrefine)
import Refined.Unsafe (unsafeRefine)


-- * Data Definition


-- | Type encoding for quantity values with a given scaling (digits after the
-- decimal point).
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
-- >>> mkQuantity 0.415 :: Quantity 2
-- 0.42
-- >>> mkQuantity 0.425 :: Quantity 2
-- 0.42
-- >>> mkQuantityLossless 0.42 :: Either String (Quantity 2)
-- Right 0.42
-- >>> mkQuantityLossless 0.415 :: Either String (Quantity 2)
-- Left "Underflow while trying to create quantity: 0.415"
newtype Quantity (s :: Nat) = MkQuantity {unQuantity :: D.Decimal D.RoundHalfEven s Integer}
  deriving (Eq, Ord, Generic, Num)


-- | Type definition for unsigned 'Quantity' values.
type UnsignedQuantity s = Refined NonNegative (Quantity s)


-- | Orphan 'TH.Lift' instance for 'Quantity'.
--
-- TODO: Avoid having an orphan instance for @Decimal r s p@?
deriving instance TH.Lift (D.Decimal D.RoundHalfEven s Integer)


-- | 'TH.Lift' instance for 'Quantity'.
deriving instance TH.Lift (Quantity s)


-- | 'Aeson.FromJSON' instance for 'Quantity'.
--
-- >>> :set -XOverloadedStrings
-- >>> Aeson.decode "0.42" :: Maybe (Quantity 2)
-- Just 0.42
-- >>> Aeson.decode "0.415" :: Maybe (Quantity 2)
-- Just 0.42
-- >>> Aeson.decode "0.425" :: Maybe (Quantity 2)
-- Just 0.42
instance KnownNat s => Aeson.FromJSON (Quantity s) where
  parseJSON = Aeson.withScientific "Quantity" (pure . mkQuantity)


-- | 'Aeson.ToJSON' instance for 'Quantity'.
--
-- >>> Aeson.encode (mkQuantity 0.42 :: Quantity 2)
-- "0.42"
instance KnownNat s => Aeson.ToJSON (Quantity s) where
  toJSON = Aeson.Number . D.toScientificDecimal . unQuantity
  toEncoding = Aeson.Encoding.scientific . D.toScientificDecimal . unQuantity


-- | Numeric arithmetic over 'Quantity' values.
--
-- >>> import Numeric.Decimal
-- >>> let a = Arith (mkQuantity 10) + Arith (mkQuantity 32) :: Arith (Quantity 2)
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
instance KnownNat s => Num (D.Arith (Quantity s)) where
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
instance KnownNat s => Fractional (D.Arith (Quantity s)) where
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


-- * Smart Constructors


-- | Constructs 'Quantity' values from 'Scientific' values in a lossy way.
--
-- This function uses 'mkQuantityAux' in case that the lossless attempt fails.
-- We could have used 'mkQuantityAux' directly. However, 'mkQuantityAux' is
-- doing too much (see 'roundScientific'). Therefore, we are first attempting a
-- lossless construction (see 'mkQuantityLossless') and we fallback to
-- 'mkQuantityAux' in case the lossless construction fails.
--
-- >>> mkQuantity 0 :: Quantity 0
-- 0
-- >>> mkQuantity 0 :: Quantity 1
-- 0.0
-- >>> mkQuantity 0 :: Quantity 2
-- 0.00
-- >>> mkQuantity 0.04 :: Quantity 1
-- 0.0
-- >>> mkQuantity 0.05 :: Quantity 1
-- 0.0
-- >>> mkQuantity 0.06 :: Quantity 1
-- 0.1
-- >>> mkQuantity 0.14 :: Quantity 1
-- 0.1
-- >>> mkQuantity 0.15 :: Quantity 1
-- 0.2
-- >>> mkQuantity 0.16 :: Quantity 1
-- 0.2
-- >>> mkQuantity 0.04 :: Quantity 2
-- 0.04
-- >>> mkQuantity 0.05 :: Quantity 2
-- 0.05
-- >>> mkQuantity 0.06 :: Quantity 2
-- 0.06
-- >>> mkQuantity 0.14 :: Quantity 2
-- 0.14
-- >>> mkQuantity 0.15 :: Quantity 2
-- 0.15
-- >>> mkQuantity 0.16 :: Quantity 2
-- 0.16
-- >>> mkQuantity 0.04 :: Quantity 3
-- 0.040
-- >>> mkQuantity 0.05 :: Quantity 3
-- 0.050
-- >>> mkQuantity 0.06 :: Quantity 3
-- 0.060
-- >>> mkQuantity 0.14 :: Quantity 3
-- 0.140
-- >>> mkQuantity 0.15 :: Quantity 3
-- 0.150
-- >>> mkQuantity 0.16 :: Quantity 3
-- 0.160
mkQuantity :: KnownNat s => Scientific -> Quantity s
mkQuantity s = case mkQuantityLossless s of
  Left _ -> mkQuantityAux s
  Right dv -> dv


-- | Constructs 'Quantity' values from 'Scientific' values in a lossy way.
--
-- >>> mkQuantityLossless 0 :: Either String (Quantity 0)
-- Right 0
-- >>> mkQuantityLossless 0 :: Either String (Quantity 1)
-- Right 0.0
-- >>> mkQuantityLossless 0 :: Either String (Quantity 2)
-- Right 0.00
-- >>> mkQuantityLossless 0.04 :: Either String (Quantity 1)
-- Left "Underflow while trying to create quantity: 4.0e-2"
-- >>> mkQuantityLossless 0.05 :: Either String (Quantity 1)
-- Left "Underflow while trying to create quantity: 5.0e-2"
-- >>> mkQuantityLossless 0.06 :: Either String (Quantity 1)
-- Left "Underflow while trying to create quantity: 6.0e-2"
-- >>> mkQuantityLossless 0.14 :: Either String (Quantity 1)
-- Left "Underflow while trying to create quantity: 0.14"
-- >>> mkQuantityLossless 0.15 :: Either String (Quantity 1)
-- Left "Underflow while trying to create quantity: 0.15"
-- >>> mkQuantityLossless 0.16 :: Either String (Quantity 1)
-- Left "Underflow while trying to create quantity: 0.16"
-- >>> mkQuantityLossless 0.04 :: Either String (Quantity 2)
-- Right 0.04
-- >>> mkQuantityLossless 0.05 :: Either String (Quantity 2)
-- Right 0.05
-- >>> mkQuantityLossless 0.06 :: Either String (Quantity 2)
-- Right 0.06
-- >>> mkQuantityLossless 0.14 :: Either String (Quantity 2)
-- Right 0.14
-- >>> mkQuantityLossless 0.15 :: Either String (Quantity 2)
-- Right 0.15
-- >>> mkQuantityLossless 0.16 :: Either String (Quantity 2)
-- Right 0.16
-- >>> mkQuantityLossless 0.04 :: Either String (Quantity 3)
-- Right 0.040
-- >>> mkQuantityLossless 0.05 :: Either String (Quantity 3)
-- Right 0.050
-- >>> mkQuantityLossless 0.06 :: Either String (Quantity 3)
-- Right 0.060
-- >>> mkQuantityLossless 0.14 :: Either String (Quantity 3)
-- Right 0.140
-- >>> mkQuantityLossless 0.15 :: Either String (Quantity 3)
-- Right 0.150
-- >>> mkQuantityLossless 0.16 :: Either String (Quantity 3)
-- Right 0.160
mkQuantityLossless :: (KnownNat s, MonadError String m) => Scientific -> m (Quantity s)
mkQuantityLossless s = either (const $ throwError ("Underflow while trying to create quantity: " <> show s)) (pure . MkQuantity) $ D.fromScientificDecimal s


-- * Utilities


-- | Rounds given quantity by @k@ digits.
--
-- >>> roundQuantity (mkQuantity 0.415 :: Quantity 3) :: Quantity 2
-- 0.42
-- >>> roundQuantity (mkQuantity 0.425 :: Quantity 3) :: Quantity 2
-- 0.42
roundQuantity :: KnownNat k => Quantity (n + k) -> Quantity n
roundQuantity (MkQuantity d) = MkQuantity (D.roundDecimal d)


-- | Multiplies two quantities with different scales and rounds back to the scale of the frst operand.
--
-- >>> times (mkQuantity 0.42 :: Quantity 2) (mkQuantity 0.42 :: Quantity 2)
-- 0.18
times :: (KnownNat s, KnownNat k) => Quantity s -> Quantity k -> Quantity s
times q1 q2 = roundQuantity (timesLossless q1 q2)


-- | Multiplies two quantities with different scales.
--
-- >>> timesLossless (mkQuantity 0.42 :: Quantity 2) (mkQuantity 0.42 :: Quantity 2)
-- 0.1764
timesLossless :: (KnownNat s, KnownNat k) => Quantity s -> Quantity k -> Quantity (s + k)
timesLossless (MkQuantity d1) (MkQuantity d2) = MkQuantity (D.timesDecimal d1 d2)


-- | Divides two quantities with same scales with possible loss.
--
-- >>> divide (mkQuantity 10 :: Quantity 2) (mkQuantity 3 :: Quantity 2)
-- Just 3.33
-- >>> divide (mkQuantity 0.42 :: Quantity 2) (mkQuantity 0 :: Quantity 2)
-- Nothing
-- >>> divide (mkQuantity 0.42 :: Quantity 2) (mkQuantity 1 :: Quantity 2)
-- Just 0.42
-- >>> divide (mkQuantity 0.42 :: Quantity 2) (mkQuantity 0.42 :: Quantity 2)
-- Just 1.00
-- >>> divide (mkQuantity 0.42 :: Quantity 2) (mkQuantity 0.21 :: Quantity 2)
-- Just 2.00
-- >>> divide (mkQuantity 0.42 :: Quantity 2) (mkQuantity (-0.21) :: Quantity 2)
-- Just -2.00
divide :: KnownNat s => Quantity s -> Quantity s -> Maybe (Quantity s)
divide (MkQuantity d1) (MkQuantity d2) = MkQuantity <$> D.divideDecimalWithRounding d1 d2


-- | Divides two quantities with different scales with possible loss preserving
-- dividend's precision.
--
-- >>> divideL (mkQuantity 10 :: Quantity 1) (mkQuantity 3 :: Quantity 2)
-- Just 3.3
-- >>> divideL (mkQuantity 10 :: Quantity 2) (mkQuantity 3 :: Quantity 2)
-- Just 3.33
-- >>> divideL (mkQuantity 10 :: Quantity 3) (mkQuantity 3 :: Quantity 2)
-- Just 3.333
divideL :: (KnownNat s, KnownNat k) => Quantity s -> Quantity k -> Maybe (Quantity s)
divideL (MkQuantity d1) d2 = MkQuantity <$> D.divideDecimalWithRounding d1 (unQuantity $ 1 `times` d2)


-- | Divides two quantities with different scales with possible loss preserving
-- divisor's precision.
--
-- >>> divideR (mkQuantity 10 :: Quantity 2) (mkQuantity 3 :: Quantity 1)
-- Just 3.3
-- >>> divideR (mkQuantity 10 :: Quantity 2) (mkQuantity 3 :: Quantity 2)
-- Just 3.33
-- >>> divideR (mkQuantity 10 :: Quantity 2) (mkQuantity 3 :: Quantity 3)
-- Just 3.333
divideR :: (KnownNat s, KnownNat k) => Quantity s -> Quantity k -> Maybe (Quantity k)
divideR d1 (MkQuantity d2) = MkQuantity <$> D.divideDecimalWithRounding (unQuantity $ 1 `times` d1) d2


-- | Divides two quantities with different scales with possible loss with a
-- target precision of result.
--
-- >>> :set -XTypeApplications
-- >>> divideD @0 (mkQuantity 10 :: Quantity 2) (mkQuantity 3 :: Quantity 2)
-- Just 3
-- >>> divideD @1 (mkQuantity 10 :: Quantity 2) (mkQuantity 3 :: Quantity 2)
-- Just 3.3
-- >>> divideD @2 (mkQuantity 10 :: Quantity 2) (mkQuantity 3 :: Quantity 2)
-- Just 3.33
-- >>> divideD @3 (mkQuantity 10 :: Quantity 2) (mkQuantity 3 :: Quantity 2)
-- Just 3.333
-- >>> divideD @8 (mkQuantity 1111 :: Quantity 2) (mkQuantity 3333 :: Quantity 12)
-- Just 0.33333333
divideD :: (KnownNat r, KnownNat s, KnownNat k) => Quantity s -> Quantity k -> Maybe (Quantity r)
divideD d1 d2 = MkQuantity <$> D.divideDecimalWithRounding (unQuantity $ 1 `times` d1) (unQuantity $ 1 `times` d2)


-- | Returns the total of a list of unsigned quantities.
--
-- >>> sumUnsignedQuantity [] :: UnsignedQuantity 2
-- Refined 0.00
sumUnsignedQuantity
  :: KnownNat s
  => [UnsignedQuantity s]
  -> UnsignedQuantity s
sumUnsignedQuantity = unsafeRefine . sum . fmap unrefine


-- | Returns the absolute value of the 'Quantity' as 'UnsignedQuantity'.
--
-- >>> abs (mkQuantity 0.42 :: Quantity 2)
-- 0.42
-- >>> abs (mkQuantity 0 :: Quantity 2)
-- 0.00
-- >>> abs (mkQuantity (-0.42) :: Quantity 2)
-- 0.42
absQuantity
  :: KnownNat s
  => Quantity s
  -> UnsignedQuantity s
absQuantity = unsafeRefine . abs


-- * Internal


-- | Auxiliary function for constructing 'Quantity' values.
--
-- See 'mkQuantity' why we need this function and why we haven't used it as the
-- direct implementation of 'mkQuantity'.
--
-- Call-sites should avoid using this function directly due to its performance
-- characteristics.
mkQuantityAux :: forall s. KnownNat s => Scientific -> Quantity s
mkQuantityAux x = fromRight err $ mkQuantityLossless (roundScientific nof x)
  where
    -- Get the term-level scaling for the target value:
    nof = fromIntegral $ natVal (Proxy :: Proxy s)

    -- This function should NOT fail in practice ever, but it can fail due to
    -- type signatures by right. We will let it error with a message for
    -- ourselves:
    err = error $ "PROGRAMMING ERROR: Can not construct 'Quantity " <> show nof <> "' with '" <> show x <> "' in a lossy way."


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
roundScientific :: Int -> Scientific -> Scientific
roundScientific = (read .) . formatScientific Fixed . Just
