-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Web.Types where

import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.TH (deriveJSON)
import Data.HashMap.Strict (HashMap)
import Entry (EntryTag, FieldContents, FieldName, FieldVisibility)
import GHC.Generics (Generic)


data NewField = NewField
  { nfContents   :: FieldContents
  , nfVisibility :: FieldVisibility
  }
  deriving stock (Show, Eq, Generic)

deriveJSON (aesonPrefix camelCase) ''NewField

data NewEntry = NewEntry
  { neFields :: HashMap FieldName NewField
  , neTags   :: [EntryTag]
  }
  deriving stock (Show, Eq, Generic)

deriveJSON (aesonPrefix camelCase) ''NewEntry
