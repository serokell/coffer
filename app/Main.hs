module Main where

import Control.Lens
import Control.Monad (forM_)
import Options.Applicative ( execParser )
import Polysemy
import Servant.Client
import Polysemy.Error (errorToIOFinal, Error)
import Data.Text (Text)
import System.Exit (die)
import Fmt

import CLI.Parser
import CLI.Types
import CLI.PrettyPrint
import Backend.Vault.Kv
import Error
import Backend.Commands as Commands
import Backend
import Coffer.Path ( Path, EntryPath )
import qualified Entry as E
import qualified Coffer.Directory as Dir

url :: BaseUrl
url = BaseUrl Http "127.0.0.1" 8200 ""

token :: VaultToken
token = VaultToken "s.AnHJuzOxRiD12eLxALFytccC"

mount :: Text
mount = "secret"

runVaultIO_ :: Sem '[BackendEffect, Error CofferError, Embed IO, Final IO ] a -> IO a
runVaultIO_ action =
  runVaultIO url token mount action
    & errorToIOFinal @CofferError
    & embedToFinal @IO
    & runFinal
    >>= \case
      Right a -> pure a
      Left err -> do
        die $ show err

main :: IO ()
main = do
  runVaultIO_ do
    embed (execParser parserInfo) >>= \case
      SomeCommand cmd@CmdView{} -> do
        runCommand cmd >>= \case
          VRDirectory dir -> pprint $ buildDirectory dir
          VREntry entry -> pprint $ buildDirectory $ Dir.singleton entry
          VRField _ field -> pprint $ build $ field ^. E.value
          VRPathNotFound path -> pathNotFound path
          VRDirectoryNoFieldMatch path fieldName -> printError $
            "There are no entries at path '" +| path |+ "' with the field '" +| fieldName |+ "'."
          VREntryNoFieldMatch path fieldName -> printError $
            "The entry at '" +| path |+ "' does not have a field '" +| fieldName |+ "'."

      SomeCommand cmd@(CmdCreate opts) -> do
        runCommand cmd >>= \case
          CRSuccess _ -> printSuccess $ "Entry created at '"  +| coPath opts |+ "'."
          CREntryAlreadyExists path -> printError $ unlinesF
            [ "An entry already exists at '" <> build path <> "'."
            , "Use '--force' or '-f' to overwrite it."
            ]

      SomeCommand cmd@(CmdSetField opts) -> do
        let fieldName = sfoFieldName opts
        runCommand cmd >>= \case
          SFREntryNotFound path -> entryNotFound path
          SFRMissingFieldContents path -> printError $ unlinesF @_ @Builder
            [ "The entry at '" +| path |+ "' does not yet have a field '" +| fieldName |+ "'."
            , "In order to create a new field, please include the 'FIELDCONTENTS' argument."
            ]
          SFRSuccess entry -> do
            let field = entry ^?! E.fields . ix fieldName
            printSuccess $
              "Set field '" +| fieldName |+
              "' to '" +| (field ^. E.value) |+
              "' (" +| (field ^. E.visibility) |+
              ") at '" +| entry ^. E.path |+ "'."

      SomeCommand cmd@(CmdDeleteField opts) -> do
        runCommand cmd >>= \case
          DFREntryNotFound path -> entryNotFound path
          DFRFieldNotFound fieldName -> printError $
            "Entry does not have a field with name '" +| fieldName |+ "'."
          DFRSuccess _ -> printSuccess $
            "Deleted field '" +| dfoFieldName opts |+ "' from '" +| dfoPath opts |+ "'."

      SomeCommand cmd@CmdFind{} -> do
        runCommand cmd >>= \case
          Just dir -> pprint $ buildDirectory dir
          Nothing -> printError "No match found."

      SomeCommand cmd@CmdRename{} -> do
        runCommand cmd >>= \case
          -- TODO: print result
          _ -> undefined
      SomeCommand cmd@CmdCopy{} -> do
        runCommand cmd >>= \case
          CPRSuccess copiedPaths ->
            forM_ copiedPaths \(from, to) ->
              printSuccess $ "Copied '" +| from |+ "' to '" +| to |+ "'."
          CPRPathNotFound path -> pathNotFound path
          CPRMissingEntryName -> printError
            "The destination path is not a valid entry path. Please specify the new name of the entry."
          CPRDestinationIsDirectory paths -> do
            let header = "The following entries cannot be copied because a directory already exists at the destination."
            let errorMsgs = paths <&> \(from, to) -> "Cannot copy '" +| from |+ "' to '" +| to |+ "'."
            printError $ unlinesF @_ @Builder $ header : "" : errorMsgs
          CPREntryAlreadyExists paths -> do
            let header = unlinesF @_ @Builder
                  [ "The following entries cannot be copied because an entry already exists at the destination."
                  , "Use '--force' or '-f' to overwrite existing entries."
                  ]
            let errorMsgs = paths <&> \(from, to) -> "Cannot copy '" +| from |+ "' to '" +| to |+ "'."
            printError $ unlinesF @_ @Builder $ header : "" : errorMsgs

      SomeCommand cmd@CmdDelete{} -> do
        runCommand cmd >>= \case
          DRPathNotFound path -> pathNotFound path
          DRDirectoryFound path -> printError $ unlinesF @_ @Builder
            [ "The path '" +| path |+ "' is a directory."
            , "Use '--recursive' or '-r' to recursively delete all entries."
            ]
          DRSuccess paths ->
            forM_ paths \path ->
              printSuccess $ "Deleted '" +| path |+ "'."

      SomeCommand cmd@(CmdTag opts) -> do
        runCommand cmd >>= \case
          TREntryNotFound path -> entryNotFound path
          TRSuccess _ ->
            if toDelete opts
              then printSuccess $ "Removed tag '" +| toTagName opts |+ "' from '" +| toPath opts |+ "'."
              else printSuccess $ "Added tag '" +| toTagName opts |+ "' to '" +| toPath opts |+ "'."
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

      entryNotFound :: Member (Embed IO) r => EntryPath -> Sem r ()
      entryNotFound path = printError $ "Entry not found at '" +| path |+ "'."

      pathNotFound :: Member (Embed IO) r => Path -> Sem r ()
      pathNotFound path = printError $ "Entry or directory not found at '" +| path |+ "'."
