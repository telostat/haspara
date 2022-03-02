{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}

module Haspara.Accounting.Account where

import Data.Hashable                  (Hashable)
import Deriving.Aeson                 (CustomJSON(CustomJSON), FromJSON, Generic, ToJSON)
import Deriving.Aeson.Stock           (PrefixedSnake)
import Haspara.Accounting.AccountKind (AccountKind)


-- | Account model.
--
-- >>> import Haspara.Accounting.AccountKind (AccountKind(..))
-- >>> import qualified Data.Aeson as Aeson
-- >>> let acc = Account AccountKindAsset (1 ::Int)
-- >>> Aeson.encode acc
-- "{\"kind\":\"ASSET\",\"object\":1}"
-- >>> Aeson.decode (Aeson.encode acc) :: Maybe (Account Int)
-- Just (Account {accountKind = AccountKindAsset, accountObject = 1})
-- >>> Aeson.decode (Aeson.encode acc) == Just acc
-- True
data Account o = Account
  { accountKind   :: !AccountKind
  , accountObject :: !o
  } deriving (Eq, Generic, Ord, Show)
  deriving (FromJSON, ToJSON)
  via PrefixedSnake "account" (Account o)


instance Hashable o => Hashable (Account o)
