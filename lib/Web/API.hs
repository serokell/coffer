-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Web.API where

import Backends (SomeBackend)
import CLI.Types
import Coffer.Directory (Directory)
import Coffer.Path (EntryPath, Path)
import Data.Text (Text)
import Entry
import GHC.Generics (Generic)
import Servant.API
import Web.Types (NewEntry)

type API
  = Header' [Required, Strict] "Coffer-Backend" SomeBackend
  :> "api" :> "v1" :> "content" :>
    ( "view"
      :> RequiredParam "path" Path
      :> Get '[JSON] Directory

    :<|> "create"
      :> RequiredParam "path" EntryPath
      :> QueryFlag     "force"
      :> ReqBody '[JSON] NewEntry
      :> Post '[JSON] Entry

    :<|> "set-field"
      :> RequiredParam "path" EntryPath
      :> RequiredParam "field" FieldName
      :> OptionalParam "visibility" FieldVisibility
      :> ReqBody '[JSON] FieldContents
      :> Post '[JSON] Entry

    :<|> "set-field-visibility"
      :> RequiredParam "path" EntryPath
      :> RequiredParam "field" FieldName
      :> ReqBody '[JSON] FieldVisibility
      :> Post '[JSON] Entry

    :<|> "delete-field"
      :> RequiredParam "path" EntryPath
      :> RequiredParam "field" FieldName
      :> Delete '[JSON] Entry

    :<|> "find"
      :> OptionalParam  "path" Path
      :> OptionalParam  "text" Text
      :> OptionalParam  "sort-field" (Sort, Direction)
      :> OptionalParams "filter" Filter
      :> Get '[JSON] (Maybe Directory)

    :<|> "rename"
      :> QueryFlag     "dry-run"
      :> RequiredParam "old-path" Path
      :> RequiredParam "new-path" Path
      :> QueryFlag     "force"
      :> Post '[JSON] [(EntryPath, EntryPath)]

    :<|> "copy"
      :> QueryFlag     "dry-run"
      :> RequiredParam "old-path" Path
      :> RequiredParam "new-path" Path
      :> QueryFlag     "force"
      :> Post '[JSON] [(EntryPath, EntryPath)]

    :<|> "delete"
      :> QueryFlag     "dry-run"
      :> RequiredParam "path" Path
      :> QueryFlag     "recursive"
      :> DeleteNoContent

    :<|> "tag"
      :> RequiredParam "path" EntryPath
      :> RequiredParam "tag" EntryTag
      :>
        (    Post   '[JSON] Entry
        :<|> Delete '[JSON] Entry
        )
    )

newtype Token = Token { getToken :: Text }
  deriving stock (Eq, Ord, Show, Generic)

type RequiredParam  = QueryParam' [Required, Strict]
type OptionalParam  = QueryParam
type OptionalParams = QueryParams
