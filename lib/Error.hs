-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Error
  ( CofferError (..)
  , BackendError
  , InternalCommandsError (..)
  ) where

import BackendName (BackendName)
import Coffer.Path (EntryPath, Path, PathSegment, unPathSegment)
import Data.Text (Text)
import Entry (BadEntryTag, BadFieldName)
import Fmt (Buildable(build))
import Text.Interpolation.Nyan

-- | GADT for coffer internal errors.
-- It is backend-agnostic, so it doesn't know about specific backend errors.
data CofferError where
  BackendError :: BackendError err => err -> CofferError
  InternalCommandsError :: InternalCommandsError -> CofferError
  BackendNotFound :: BackendName -> CofferError
  BadFieldNameError :: BadFieldName -> CofferError
  BadMasterFieldName :: Text -> BadFieldName -> CofferError
  BadEntryTagError :: BadEntryTag -> CofferError
  InvalidPathSegment
    :: PathSegment
    -> Text -- ^ Backend-specific error message
    -> CofferError

-- | Type class for backend errors.
class (Buildable err) => BackendError err

-- | Internal errors that can be thrown if backend is in illegal state.
data InternalCommandsError
  = EntryPathDoesntHavePrefix EntryPath Path

instance Buildable InternalCommandsError where
  build = \case
    EntryPathDoesntHavePrefix entryPath path ->
      [int|s|
        Expected path: '#{entryPath}'
        To have the prefix: '#{path}'
      |]

instance Buildable CofferError where
  build = \case
    BackendError err ->
      [int|s|
        Internal backend error:
        #{err}
      |]
    InternalCommandsError err ->
      [int|s|
        Internal error:
        #{err}
      |]
    BackendNotFound backendName ->
      [int|s|Backend with name '#{backendName}' not found.|]
    InvalidPathSegment segment errMsg ->
      [int|s|
        Invalid path segment for target backend:
        Got: #s{unPathSegment segment}.

        #{errMsg}
      |]
    BadFieldNameError err -> build err
    BadMasterFieldName name err ->
      [int|s|
        Attempted to create new field name from '#{name}'

        #{err}
      |]
    BadEntryTagError err -> build err
