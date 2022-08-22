-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Web.Types where

import Coffer.Path (EntryPath, exampleEntryPath, mkEntryPath)
import Control.Lens
import Data.Aeson (toJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.TH (deriveJSON, deriveToJSON)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HS
import Data.OpenApi
import Data.Text qualified as T
import Entry
  (EntryTag, FieldContents(..), FieldName, FieldVisibility(..), exampleEntryTag, newFieldName)
import Fmt (pretty)
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
  declareNamedSchema proxy =
    genericDeclareNamedSchema (fromAesonOptions (aesonPrefix camelCase)) proxy
      & mapped . schema . example ?~ toJSON exampleNewEntry
    where
      exampleNewEntry :: NewEntry
      exampleNewEntry =
        NewEntry
        { neFields = HS.fromList
            [ ( either (error . pretty) id $ newFieldName "username"
              , NewField
                  { nfVisibility = Public
                  , nfContents = FieldContents "some-username"
                  }
              )
            , ( either (error . pretty) id $ newFieldName "password"
              , NewField
                  { nfVisibility = Private
                  , nfContents = FieldContents "some-password"
                  }
              )
            ]
        , neTags = [exampleEntryTag]
        }

-- | Datatype that serves as a workaround for this issue:
-- https://github.com/biocad/openapi3/issues/31
data CopiedEntry = CopiedEntry
  { poFrom :: EntryPath
  , poTo :: EntryPath
   }
  deriving stock (Show, Eq, Generic)

deriveToJSON (aesonPrefix camelCase) ''CopiedEntry

mkCopiedEntry :: (EntryPath, EntryPath) -> CopiedEntry
mkCopiedEntry (a, b) = CopiedEntry a b

instance ToSchema CopiedEntry where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (fromAesonOptions (aesonPrefix camelCase)) proxy
      & mapped . schema . example ?~ toJSON CopiedEntry
        { poFrom = exampleEntryPath
        , poTo = either (error . T.unpack) id $ mkEntryPath "/accounts/sre-backup/gmail"
        }
