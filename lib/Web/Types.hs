-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Web.Types where

import Data.Aeson qualified as A
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.TH (deriveJSON)
import Data.HashMap.Strict (HashMap)
import Data.OpenApi
import Entry (EntryTag, FieldContents, FieldName, FieldVisibility)
import GHC.Generics (Generic)

data NewField = NewField
  { nfContents   :: FieldContents
  , nfVisibility :: FieldVisibility
  }
  deriving stock (Show, Eq, Generic)
deriveJSON (aesonPrefix camelCase) ''NewField

instance ToSchema NewField where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions (aesonPrefix camelCase)

data NewEntry = NewEntry
  { neFields :: HashMap FieldName NewField
  , neTags   :: [EntryTag]
  }
  deriving stock (Show, Eq, Generic)
deriveJSON (aesonPrefix camelCase) ''NewEntry

instance ToSchema NewEntry where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions (aesonPrefix camelCase)

-- | Datatype that serves as a workaround for this issue:
-- https://github.com/biocad/openapi3/issues/31
data PairObject a = PairObject
  { poFirst :: a
  , poSecond :: a
   }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)

mkPair :: (a, a) -> PairObject a
mkPair (a, b) = PairObject a b

instance ToSchema a =>  ToSchema (PairObject a) where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions (aesonPrefix camelCase)
