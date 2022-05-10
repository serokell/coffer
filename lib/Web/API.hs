-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Web.API where

import Backend.Vault.Kv
import CLI.Types
import Coffer.Directory (Directory)
import Coffer.Path (EntryPath, Path, QualifiedPath)
import Data.Text (Text)
import Entry
import GHC.Generics (Generic)
import Servant.API
import Web.Types (NewEntry)

type API
  = Header' [Required, Strict] "token" VaultToken
  :> "api" :> "v1" :> "content" :>
    ( "view"
      :> RequiredParam "path"  (QualifiedPath Path)
      :> OptionalParam "field" FieldName
      :> Get '[JSON] ViewResult

    :<|> "create"
      :> RequiredParam "path" (QualifiedPath EntryPath)
      :> QueryFlag     "force"
      :> ReqBody '[JSON] NewEntry
      :> Post '[JSON] CreateResult

    :<|> "set-field"
      :> RequiredParam "path" (QualifiedPath EntryPath)
      :> RequiredParam "field" FieldName
      :>
        (    "private" :> Post '[JSON] SetFieldResult
        :<|> "public"  :> Post '[JSON] SetFieldResult
        :<|> ReqBody '[JSON] (Maybe FieldContents)
          :> Post '[JSON] SetFieldResult
        )

    :<|> "delete-field"
      :> RequiredParam "path" (QualifiedPath EntryPath)
      :> RequiredParam "field" FieldName
      :> Delete '[JSON] DeleteFieldResult

    :<|> "find"
      :> OptionalParam  "path" (QualifiedPath Path)
      :> OptionalParam  "text" Text
      :> OptionalParam  "sort-field" (Sort, Direction)
      :> OptionalParams "filter" Filter
      :> Get '[JSON] (Maybe Directory)

    :<|> "rename"
      :> QueryFlag     "dry-run"
      :> RequiredParam "old-path" (QualifiedPath Path)
      :> RequiredParam "new-path" (QualifiedPath Path)
      :> QueryFlag     "force"
      :> Post '[JSON] RenameResult

    :<|> "copy"
      :> QueryFlag     "dry-run"
      :> RequiredParam "old-path" (QualifiedPath Path)
      :> RequiredParam "new-path" (QualifiedPath Path)
      :> QueryFlag     "force"
      :> Post '[JSON] CopyResult

    :<|> "delete"
      :> QueryFlag     "dry-run"
      :> RequiredParam "path" (QualifiedPath Path)
      :> QueryFlag     "recursive"
      :> Delete '[JSON] DeleteResult

    :<|> "tag"
      :> RequiredParam "path" (QualifiedPath EntryPath)
      :> RequiredParam "tag" EntryTag
      :> QueryFlag     "delete"
      :> Post '[JSON] TagResult
    )

newtype Token = Token { getToken :: Text }
  deriving stock (Eq, Ord, Show, Generic)

type RequiredParam  = QueryParam' [Required, Strict]
type OptionalParam  = QueryParam
type OptionalParams = QueryParams
