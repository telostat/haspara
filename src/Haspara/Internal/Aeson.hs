-- | This module provides helper definitions for "Data.Aeson".
module Haspara.Internal.Aeson where

import qualified Data.Aeson as Aeson
import qualified Data.Char as C
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)


-- | Common Aeson encoding/decoding options.
commonAesonOptions :: String -> Aeson.Options
commonAesonOptions prefix =
  Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    , Aeson.fieldLabelModifier = \l -> Aeson.camelTo2 '_' . fromMaybe l $ stripPrefix prefix l
    , Aeson.constructorTagModifier = \l -> Aeson.camelTo2 '_' . fromMaybe l $ stripPrefix prefix l
    , Aeson.sumEncoding =
        Aeson.TaggedObject
          { Aeson.tagFieldName = "type"
          , Aeson.contentsFieldName = "value"
          }
    }


-- | Aeson encoding/decoding options for uppercase constructor tag modifiers
aesonOptionsForSingleTag :: String -> Aeson.Options
aesonOptionsForSingleTag prefix =
  Aeson.defaultOptions
    { Aeson.constructorTagModifier = \l -> fmap C.toUpper . Aeson.camelTo2 '_' . fromMaybe l $ stripPrefix prefix l
    }
