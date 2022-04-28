-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Error
  ( CofferError (..)
  , BackendError
  , InternalCommandsError (..)
  ) where

import BackendName (BackendName)
import Coffer.Path (EntryPath, Path)
import Data.Text (Text)
import Entry (BadEntryTag, BadFieldName)
import Fmt (Buildable(build), Builder, pretty, unlinesF, (+|), (|+))

-- | GADT for coffer internal errors.
-- It is backend-agnostic, so it doesn't know about specific backend errors.
data CofferError where
  BackendError :: BackendError err => err -> CofferError
  InternalCommandsError :: InternalCommandsError -> CofferError
  BackendNotFound :: BackendName -> CofferError
  BadFieldNameError :: BadFieldName -> CofferError
  BadMasterFieldName :: Text -> BadFieldName -> CofferError
  BadEntryTagError :: BadEntryTag -> CofferError

-- | Type class for backend errors.
class (Buildable err) => BackendError err

-- | Internal errors that can be thrown if backend is in illegal state.
data InternalCommandsError
  = InvalidEntry Text
  | EntryPathDoesntHavePrefix EntryPath Path

instance Buildable InternalCommandsError where
  build = \case
    InvalidEntry entry ->
      unlinesF @_ @Builder
        [ "Backend returned a secret that is not a valid\
          \ entry or directory name."
        , "Got: '" +| entry |+ "'."
        ]
    EntryPathDoesntHavePrefix entryPath path ->
      unlinesF @_ @Builder
        [ "Expected path: '" <> pretty entryPath <> "'"
        , "To have the prefix: '" <> pretty path <> "'"
        ]

instance Buildable CofferError where
  build = \case
    BackendError err ->
      unlinesF @_ @Builder
        [ "Internal backend error:"
        , build err
        ]
    InternalCommandsError err ->
      unlinesF @_ @Builder
        [ "Internal error:"
        , build err
        ]
    BackendNotFound backendName ->
      "Backend with name '" <> build backendName <> "' not found."
    BadFieldNameError err -> build err
    BadMasterFieldName name err ->
      unlinesF @_ @Builder
        [ "Attempted to create new field name from '" +| name |+ "'"
        , ""
        , build err
        ]
    BadEntryTagError err -> build err
