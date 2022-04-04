-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Main where

import Backend
import Backend.Commands as Commands
import Backend.Interpreter
import CLI.Parser
import CLI.PrettyPrint
import CLI.Types
import Coffer.Directory qualified as Dir
import Coffer.Path (EntryPath, Path, QualifiedPath(qpPath))
import Config (Config(..), configCodec)
import Control.Lens
import Control.Monad (forM, forM_, when)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Text.IO qualified as TIO
import Entry qualified as E
import Error
import Fmt
import Options.Applicative (execParser)
import Polysemy
import Polysemy.Error (Error, errorToIOFinal)
import System.Environment (lookupEnv)
import System.Exit (die, exitFailure)
import Toml qualified

runBackendIO
  :: Sem '[BackendEffect, Error CofferError, Embed IO, Final IO ] a
  -> IO a
runBackendIO action =
  runBackend action
    & errorToIOFinal @CofferError
    & embedToFinal @IO
    & runFinal
    >>= \case
      Right a -> pure a
      Left err -> do
        die $ show err

readConfig :: FilePath -> IO Config
readConfig configPath = do
  text <- TIO.readFile configPath
  let config = Toml.decode configCodec text
  case config of
    Left err -> do
      TIO.putStrLn $ "An error occurred while decoding '" <> pack configPath <> "':"
      TIO.putStrLn $ Toml.prettyTomlDecodeErrors err
      exitFailure
    Right config -> pure config

getConfigPath :: Options -> IO FilePath
getConfigPath options = do
  case oConfigPathMb options of
    Just configPath -> pure configPath
    Nothing -> do
      envConfigPathMb <- lookupEnv "COFFER_CONFIG"
      pure $ fromMaybe "config.toml" envConfigPathMb

main :: IO ()
main = do
  options <- execParser parserInfo
  let someCommand = oSomeCommand options
  configPath <- getConfigPath options
  config <- readConfig configPath
  runBackendIO do
    case someCommand of
      SomeCommand cmd@CmdView{} -> do
        runCommand config cmd >>= \case
          VRDirectory dir -> pprint $ buildDirectory dir
          VREntry entry -> pprint $ buildDirectory $ Dir.singleton entry
          VRField _ field -> pprint $ build $ field ^. E.value
          VRPathNotFound path -> pathNotFound path
          VRDirectoryNoFieldMatch path fieldName -> printError $
            "There are no entries at path '" +| path |+ "' with the field '" +| fieldName |+ "'."
          VREntryNoFieldMatch path fieldName -> printError $
            "The entry at '" +| path |+ "' does not have a field '" +| fieldName |+ "'."

      SomeCommand cmd@(CmdCreate opts) -> do
        runCommand config cmd >>= \case
          CRSuccess _ -> printSuccess $ "Entry created at '"  +| coQPath opts |+ "'."
          CRCreateError error -> do
            let errorMsg = createErrorToBuilder error
            printError $ unlinesF @_ @Builder $ "The entry cannot be created:" : "" : [errorMsg]

      SomeCommand cmd@(CmdSetField opts) -> do
        let fieldName = sfoFieldName opts
        runCommand config cmd >>= \case
          SFREntryNotFound path -> entryNotFound path
          SFRMissingFieldContents path -> printError $ unlinesF @_ @Builder
            [ "The entry at '" +| path |+ "' does not yet have a field '" +| fieldName |+ "'."
            , "In order to create a new field, please include the 'FIELDCONTENTS' argument."
            ]
          SFRSuccess qEntry -> do
            let entry = qpPath qEntry
            let qPath = view E.path <$> qEntry
            let field = entry ^?! E.fields . ix fieldName
            printSuccess $
              "Set field '" +| fieldName |+
              "' (" +| (field ^. E.visibility) |+
              ") at '" +| qPath |+
              "' to:\n" +| (field ^. E.value) |+ ""

      SomeCommand cmd@(CmdDeleteField opts) -> do
        runCommand config cmd >>= \case
          DFREntryNotFound path -> entryNotFound path
          DFRFieldNotFound fieldName -> printError $
            "Entry does not have a field with name '" +| fieldName |+ "'."
          DFRSuccess _ -> printSuccess $
            "Deleted field '" +| dfoFieldName opts |+ "' from '" +| dfoQPath opts |+ "'."

      SomeCommand cmd@CmdFind{} -> do
        runCommand config cmd >>= \case
          Just dir -> pprint $ buildDirectory dir
          Nothing -> printError "No match found."

      SomeCommand cmd@(CmdRename opts) -> do
        runCommand config cmd >>= \case
          CPRSuccess copiedPaths -> do
            when (roDryRun opts) do
              pprint "These actions would be done:"
            forM_ copiedPaths \(from, to) ->
              printSuccess $ "Renamed '" +| from |+ "' to '" +| to |+ "'."
          CPRPathNotFound path -> pathNotFound path
          CPRMissingEntryName -> printError
            "The destination path is not a valid entry path. Please specify the new name of the entry."
          CPRCreateErrors errors -> do
            errorMsgs <- buildErrorMessages errors
            printError $ unlinesF @_ @Builder $ "The following entries cannot be renamed:" : "" : errorMsgs

      SomeCommand cmd@(CmdCopy opts) -> do
        runCommand config cmd >>= \case
          CPRSuccess copiedPaths -> do
            when (cpoDryRun opts) do
              pprint "These actions would be done:"
            forM_ copiedPaths \(from, to) ->
              printSuccess $ "Copied '" +| from |+ "' to '" +| to |+ "'."
          CPRPathNotFound path -> pathNotFound path
          CPRMissingEntryName -> printError
            "The destination path is not a valid entry path. Please specify the new name of the entry."
          CPRCreateErrors errors -> do
            errorMsgs <- buildErrorMessages errors
            printError $ unlinesF @_ @Builder $ "The following entries cannot be copied:" : "" : errorMsgs

      SomeCommand cmd@(CmdDelete opts) -> do
        runCommand config cmd >>= \case
          DRPathNotFound path -> pathNotFound path
          DRDirectoryFound path -> printError $ unlinesF @_ @Builder
            [ "The path '" +| path |+ "' is a directory."
            , "Use '--recursive' or '-r' to recursively delete all entries."
            ]
          DRSuccess paths -> do
            when (doDryRun opts) do
              pprint "These actions would be done:"
            forM_ paths \path ->
              printSuccess $ "Deleted '" +| path |+ "'."

      SomeCommand cmd@(CmdTag opts) -> do
        runCommand config cmd >>= \case
          TREntryNotFound path -> entryNotFound path
          TRSuccess _ ->
            if toDelete opts
              then printSuccess $ "Removed tag '" +| toTagName opts |+ "' from '" +| toQPath opts |+ "'."
              else printSuccess $ "Added tag '" +| toTagName opts |+ "' to '" +| toQPath opts |+ "'."
          TRTagNotFound tag -> printError $
            "Entry does not have the tag '" +| tag |+ "'."
          TRDuplicateTag tag -> printError $
            "Entry already has the tag '" +| tag |+ "'."
    where
      -- | Pretty-print a message.
      pprint :: Member (Embed IO) r => Builder -> Sem r ()
      pprint = embed . putStrLn . fmt

      -- | Print a message indicating a successful operation.
      printSuccess :: Member (Embed IO) r => Builder -> Sem r ()
      printSuccess msg = embed $ putStrLn $ "[SUCCESS] " <> fmt msg

      -- | Print a message and exit with `exitFailure`.
      printError :: Member (Embed IO) r => Builder -> Sem r ()
      printError msg = embed $ die $ "[ERROR] " <> fmt msg

      entryNotFound :: Member (Embed IO) r => QualifiedPath EntryPath -> Sem r ()
      entryNotFound path = printError $ "Entry not found at '" +| path |+ "'."

      pathNotFound :: Member (Embed IO) r => QualifiedPath Path -> Sem r ()
      pathNotFound path = printError $ "Entry or directory not found at '" +| path |+ "'."

      createErrorToBuilder :: CreateError -> Builder
      createErrorToBuilder = \case
        CEEntryAlreadyExists entryPath -> unlinesF @_ @Builder
          [ "An entry already exists at '" +| entryPath |+ "'."
          , "Use '--force' or '-f' to overwrite existing entries."
          ]
        CEDestinationIsDirectory entryPath -> "'" +| entryPath |+ "' is a directory."
        CEParentDirectoryIsEntry (_, clashed) ->
          "Attempted to create the directory '" +| clashed |+ "' but an entry exists at that path."

      getEntryFromCreateError :: CreateError -> QualifiedPath EntryPath
      getEntryFromCreateError = \case
        CEParentDirectoryIsEntry (entryPath, _) -> entryPath
        CEDestinationIsDirectory entryPath -> entryPath
        CEEntryAlreadyExists entryPath -> entryPath

      buildErrorMessages :: [(QualifiedPath EntryPath, CreateError)] -> Sem r [Builder]
      buildErrorMessages errors = do
        forM errors \(from, err) -> do
          let entryPath = getEntryFromCreateError err
          let header = "'" +| from |+ "' to '" +| entryPath |+ "':"
          let errorMsg = createErrorToBuilder err
          pure $ unlinesF @_ @Builder $ header : [indentF 2 errorMsg]
