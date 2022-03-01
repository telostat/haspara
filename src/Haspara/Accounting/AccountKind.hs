{-# LANGUAGE DeriveGeneric #-}

module Haspara.Accounting.AccountKind where

import qualified Data.Aeson    as Aeson
import qualified Data.Char     as C
import           Data.Hashable (Hashable)
import qualified Data.Text     as T
import           GHC.Generics  (Generic)


data AccountKind =
    AccountKindAsset
  | AccountKindLiability
  | AccountKindEquity
  | AccountKindRevenue
  | AccountKindExpense
  deriving (Enum, Eq, Generic, Ord, Show)


instance Hashable AccountKind


-- | 'Aeson.FromJSON' instance for 'AccountKind'.
--
-- >>> Aeson.decode "\"Asset\"" :: Maybe AccountKind
-- Just AccountKindAsset
-- >>> Aeson.decode "\"aSSET\"" :: Maybe AccountKind
-- Just AccountKindAsset
-- >>> Aeson.decode "\"ASSET\"" :: Maybe AccountKind
-- Just AccountKindAsset
-- >>> Aeson.decode "\"LIABILITY\"" :: Maybe AccountKind
-- Just AccountKindLiability
-- >>> Aeson.decode "\"EQUITY\"" :: Maybe AccountKind
-- Just AccountKindEquity
-- >>> Aeson.decode "\"REVENUE\"" :: Maybe AccountKind
-- Just AccountKindRevenue
-- >>> Aeson.decode "\"EXPENSE\"" :: Maybe AccountKind
-- Just AccountKindExpense
instance Aeson.FromJSON AccountKind where
  parseJSON = Aeson.withText "AccountKind" $ \t -> case T.map C.toUpper t of
    "ASSET"     -> pure AccountKindAsset
    "LIABILITY" -> pure AccountKindLiability
    "EQUITY"    -> pure AccountKindEquity
    "REVENUE"   -> pure AccountKindRevenue
    "EXPENSE"   -> pure AccountKindExpense
    _           -> fail $ "Unknown account kind: " <> show t


-- | 'Aeson.ToJSON' instance for 'AccountKind'.
--
-- >>> Aeson.encode AccountKindAsset
-- "\"ASSET\""
-- >>> Aeson.encode AccountKindLiability
-- "\"LIABILITY\""
-- >>> Aeson.encode AccountKindEquity
-- "\"EQUITY\""
-- >>> Aeson.encode AccountKindRevenue
-- "\"REVENUE\""
-- >>> Aeson.encode AccountKindExpense
-- "\"EXPENSE\""
instance Aeson.ToJSON AccountKind where
  toJSON AccountKindAsset     = Aeson.String "ASSET"
  toJSON AccountKindLiability = Aeson.String "LIABILITY"
  toJSON AccountKindEquity    = Aeson.String "EQUITY"
  toJSON AccountKindRevenue   = Aeson.String "REVENUE"
  toJSON AccountKindExpense   = Aeson.String "EXPENSE"


accountKindText :: AccountKind -> T.Text
accountKindText AccountKindAsset     = "Asset"
accountKindText AccountKindLiability = "Liability"
accountKindText AccountKindEquity    = "Equity"
accountKindText AccountKindRevenue   = "Revenue"
accountKindText AccountKindExpense   = "Expense"
