{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haspara.Internal.Id where

import qualified Data.Aeson          as Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Hashable       (Hashable)


-- | Type encoding for entity identifiers.
--
-- This encoding allows us to provide a phantom type for distinguishing between
-- identifiers of varying types and an underlying identifier type.
--
-- For example:
--
-- >>> data A = A
-- >>> data B = B
-- >>> data C = C
-- >>> type IdA = Id A Int
-- >>> type IdB = Id B Int
-- >>> type IdC = Id C String
-- >>> let idA = Id 1 :: IdA
-- >>> let idB = Id 1 :: IdB
-- >>> let idC = Id "C1" :: IdC
-- >>> idA
-- 1
-- >>> idB
-- 1
-- >>> idC
-- "C1"
-- >>> idA == idA
-- True
-- >>> -- idA == idB  -- Compile error as: Couldn't match type ‘B’ with ‘A’
--
-- Hashes, on the otherhand, can be compared:
--
-- >>> import Data.Hashable
-- >>> hash idA == hash idB
-- True
newtype Id a b = Id { unId :: b }
  deriving(Eq, Ord, Hashable)


instance (Show b) => Show (Id a b) where
  show (Id x) = show x


instance (Aeson.FromJSON b) => Aeson.FromJSON (Id a b) where
  parseJSON = fmap Id . Aeson.parseJSON


instance (Aeson.ToJSON b) => Aeson.ToJSON (Id a b) where
  toJSON (Id x) = Aeson.toJSON x


-- | Type encoding for a lookup table from entity 'Id's to corresponding entities.
--
-- >>> data A = A Int String deriving Show
-- >>> type IdA = Id A Int
-- >>> let a1 = A 1 "a1"
-- >>> let a2 = A 2 "a2"
-- >>> let a3 = A 3 "a3"
-- >>> let table = HM.fromList [(Id 1, a1), (Id 2, a2), (Id 3, a3)] :: IdLookup A Int
-- >>> HM.lookup (Id 1) table
-- Just (A 1 "a1")
type IdLookup a b = HM.HashMap (Id a b) a
