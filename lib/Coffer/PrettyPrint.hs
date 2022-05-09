-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Coffer.PrettyPrint where

import CLI.PrettyPrint (buildDirectory)
import CLI.Types
import Coffer.Directory qualified as Dir
import Coffer.Path (EntryPath, Path, QualifiedPath(qpPath))
import Control.Lens
import Data.Text (Text)
import Data.Text qualified as T
import Entry qualified as E
import Fmt (Buildable(build), Builder, fmt, indentF, pretty, unlinesF)
import Text.Interpolation.Nyan

data PrettyPrintMode
  = CLI
  | WebAPI
  deriving stock (Show)

buildPathNotFound :: QualifiedPath Path -> Builder
buildPathNotFound path = [int|s|Entry or directory not found at '#{path}'.|]

buildEntryNotFound :: QualifiedPath EntryPath -> Builder
buildEntryNotFound entryPath = [int|s|Entry not found at '#{entryPath}'.|]

buildSamePath :: QualifiedPath Path -> Builder
buildSamePath path = [int|s|'#{path}' and '#{path}' are the same path.|]

buildViewResult :: PrettyPrintMode -> ViewResult -> Builder
buildViewResult _ = \case
  VRDirectory dir -> buildDirectory dir
  VREntry entry -> buildDirectory $ Dir.singleton entry
  VRField _ field -> build $ field ^. E.contents
  VRPathNotFound path -> buildPathNotFound path
  VRDirectoryNoFieldMatch path fieldName -> [int|s|
      There are no entries at path '#{path}' with the field '#{fieldName}'.
    |]
  VREntryNoFieldMatch path fieldName -> [int|s|
      The entry at '#{path}' does not have a field '#{fieldName}'.
    |]

buildCreateError :: PrettyPrintMode -> CreateError -> Builder
buildCreateError mode = \case
  CEEntryAlreadyExists entryPath -> do
    let forceMessage :: Builder =
          case mode of
            CLI -> "Use '--force' or '-f' to overwrite existing entries."
            WebAPI -> "Use 'force' query flag to overwrite existing entries."
    [int|s|
      An entry already exists at '#{entryPath}'.
      #{forceMessage}
    |]
  CEDestinationIsDirectory entryPath -> [int|s|'#{entryPath}' is a directory.|]
  CEParentDirectoryIsEntry (_, clashed) ->
    [int|s|Attempted to create the directory '#{clashed}' but an entry exists at that path.|]

buildCreateResult :: PrettyPrintMode -> CreateResult -> Builder
buildCreateResult mode = \case
  CRSuccess path -> [int|s|Entry created at '#{path}'.|]
  CRCreateError error -> [int|s|
      The entry cannot be created:

      #{buildCreateError mode error}
    |]

buildSetFieldResult :: PrettyPrintMode -> SetFieldResult -> Builder
buildSetFieldResult mode = \case
  SFREntryNotFound path -> buildEntryNotFound path
  SFRMissingFieldContents fieldName path -> do
    let fieldContentsMessage :: Builder =
          case mode of
            CLI -> "In order to create a new field, please include the 'FIELDCONTENTS' argument."
            WebAPI -> "In order to create a new field, please include 'FIELDCONTENTS' in the body."
    [int|s|
      The entry at '#{path}' does not yet have a field '#{fieldName}'.
      #{fieldContentsMessage}
    |]
  SFRSuccess fieldName qEntry -> do
    let entry = qpPath qEntry
    let qPath = view E.path <$> qEntry
    let field = entry ^?! E.fields . ix fieldName
    [int|s|
      Set field '#{fieldName}' (#{field ^. E.visibility}) \
      at '#{qPath}' to:
      #{field ^. E.contents}
    |]

buildDeleteFieldResult :: PrettyPrintMode -> DeleteFieldResult -> Builder
buildDeleteFieldResult _ = \case
  DFREntryNotFound path -> buildEntryNotFound path
  DFRFieldNotFound fieldName -> [int|s|
      Entry does not have a field with name '#{fieldName}'.
    |]
  DFRSuccess fieldName path -> [int|s|
      Deleted field '#{fieldName}' from '#{path}'.
    |]

getEntryFromCreateError :: CreateError -> QualifiedPath EntryPath
getEntryFromCreateError = \case
  CEParentDirectoryIsEntry (entryPath, _) -> entryPath
  CEDestinationIsDirectory entryPath -> entryPath
  CEEntryAlreadyExists entryPath -> entryPath

buildErrorMessages :: PrettyPrintMode -> [(QualifiedPath EntryPath, CreateError)] -> [Builder]
buildErrorMessages mode =
  fmap \(from, err) ->
    let
      entryPath = getEntryFromCreateError err
      header = [int|s|'#{from}' to '#{entryPath}':|]
      errorMsg = buildCreateError mode err
    in unlinesF @_ @Builder $ header : [indentF 2 errorMsg]

buildCopyOrRenameResult :: Bool -> PrettyPrintMode -> CopyResult -> [Builder]
buildCopyOrRenameResult rename mode = \case
  CPRSuccess dryRun copiedPaths -> do
    let pref :: Builder = if rename then "Renamed" else "Copied"
    let messages = copiedPaths
          & map do \(from, to) -> [int|s|[SUCCESS] #{pref} '#{from}' to '#{to}'.|]
          & unlinesF @_ @Builder
    if dryRun
      then [[int|s|
        These actions would be done:
        #{messages}
      |]]
      else [messages]
  CPRPathNotFound path -> [buildPathNotFound path]
  CPRMissingEntryName ->
    ["The destination path is not a valid entry path. Please specify the new name of the entry."]
  CPRCreateErrors errors -> buildErrorMessages mode errors
  CPRSamePath path -> [buildSamePath path]

buildCopyResult :: PrettyPrintMode -> CopyResult -> [Builder]
buildCopyResult = buildCopyOrRenameResult False

buildRenameResult :: PrettyPrintMode -> RenameResult -> [Builder]
buildRenameResult = buildCopyOrRenameResult True

buildDeleteResult :: PrettyPrintMode -> DeleteResult -> Builder
buildDeleteResult mode = \case
  DRPathNotFound path -> buildPathNotFound path
  DRDirectoryFound path -> do
    let recursiveMessage :: Builder =
          case mode of
            CLI -> "Use '--recursive' or '-r' to recursively delete all entries."
            WebAPI -> "Use 'recursive' query flag to recursively delete all entries."
    [int|s|
      The path '#{path}' is a directory.
      #{recursiveMessage}
    |]
  DRSuccess dryRun paths -> do
    let messages = paths
          & map do \path -> [int|s|[SUCCESS] Deleted '#{path}'.|]
          & unlinesF @_ @Builder
          & fmt @Text
          & T.stripEnd
    if dryRun
      then [int|s|
        These actions would be done:
        #{messages}
      |]
      else pretty messages

buildTagResult :: PrettyPrintMode -> TagResult -> Builder
buildTagResult _ = \case
  TREntryNotFound path -> buildEntryNotFound path
  TRSuccess path tagName delete ->
    if delete
      then [int|s|Removed tag '#{tagName}' from '#{path}'.|]
      else [int|s|Added tag '#{tagName}' to '#{path}'.|]
  TRTagNotFound tag ->
    [int|s|Entry does not have the tag '#{tag}'.|]
  TRDuplicateTag tag ->
    [int|s|Entry already has the tag '#{tag}'.|]
