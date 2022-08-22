-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Web.API where

import Backends (SomeBackend)
import CLI.Types (Direction, Filter, Sort)
import Coffer.Directory (Directory)
import Coffer.Path (EntryPath, Path)
import Data.Text (Text)
import Entry
import Servant.API
import Web.Types (CopiedEntry, NewEntry)

-- Note: We can't yet add swagger docs to `QueryParams` and `QueryFlag`.
-- See: https://github.com/haskell-servant/servant/issues/1602

type API
  = RequiredHeaderDesc "Coffer-Backend" SomeBackend "Details about the backend to connect to."
  :> "api" :> "v1" :> "content" :>
    ( "view"
      :> RequiredParamDesc "path" Path "A path to either a directory or an entry."
      :> Get '[JSON] Directory

    :<|> "create"
      :> RequiredParamDesc "path" EntryPath
          "The path of the new entry. This must not already be a directory or an entry.\
          \ If it is an entry, `force` can be used to overwrite it."
      :> QueryFlag "force"
      :> ReqBody '[JSON] NewEntry
      :> Post '[JSON] Entry

    :<|> "set-field"
      :> RequiredParamDesc "path" EntryPath "The path of the entry to be modified."
      :> RequiredParamDesc "field" FieldName "The field to be added/modified."
      :>
        (    "private" :> Post '[JSON] Entry
        :<|> "public"  :> Post '[JSON] Entry
        :<|> ReqBody '[JSON] (Maybe FieldContents)
          :> Post '[JSON] Entry
        )

    :<|> "delete-field"
      :> RequiredParamDesc "path" EntryPath "The path of the entry to be modified."
      :> RequiredParamDesc "field" FieldName "The field to be deleted"
      :> Delete '[JSON] Entry

    :<|> "find"
      :> OptionalParamDesc "path" Path "If specified, only show entries within this path (use `/` to find everything)."
      :> OptionalParamDesc "text" Text "The text to search for in the paths and tags of entries."
      :> OptionalParamDesc "sort-field" (Sort, Direction) "Sort the entries inside each directory."
      :> OptionalParams "filter" Filter
      :> Get '[JSON] (Maybe Directory)

    :<|> "rename"
      :> QueryFlag "dry-run"
      :> RequiredParamDesc "old-path" Path "The path of the directory or entry to be moved."
      :> RequiredParamDesc "new-path" Path "The path to move the directory or entry to."
      :> QueryFlag     "force"
      :> Post '[JSON] [CopiedEntry]

    :<|> "copy"
      :> QueryFlag "dry-run"
      :> RequiredParamDesc "old-path" Path "The path of the directory or entry to be copied."
      :> RequiredParamDesc "new-path" Path "The path to copy the directory or entry to."
      :> QueryFlag "force"
      :> Post '[JSON] [CopiedEntry]

    :<|> "delete"
      :> QueryFlag "dry-run"
      :> RequiredParamDesc "path" Path "The path of the entry or directory to delete."
      :> QueryFlag "recursive"
      :> DeleteNoContent

    :<|> "tag"
      :>
        (
          RequiredParamDesc "path" EntryPath "The path of the entry to add a tag to."
          :> RequiredParamDesc "tag" EntryTag "The name of the tag to add."
          :> Post '[JSON] Entry
        :<|>
          RequiredParamDesc "path" EntryPath "The path of the entry to delete a tag from."
          :> RequiredParamDesc "tag" EntryTag "The name of the tag to delete."
          :> Delete '[JSON] Entry
        )
    )

type RequiredHeaderDesc name ty desc = Header' [Required, Strict, Description desc] name ty
type RequiredParamDesc name ty desc = QueryParam' [Required, Strict, Description desc] name ty
type OptionalParamDesc name ty desc = QueryParam' [Optional, Strict, Description desc] name ty
type OptionalParams = QueryParams
