-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Main where

import Backend.Commands as Commands
import Backend.Interpreter
import BackendEffect
import CLI.Parser
import CLI.PrettyPrint
import CLI.Types
import Coffer.PrettyPrint
  (PrettyPrintMode(CLI), buildCopyResult, buildCreateResult, buildDeleteFieldResult,
  buildDeleteResult, buildRenameResult, buildSetFieldResult, buildSetFieldVisibilityResult,
  buildTagResult, buildViewResult)
import Config (Config(..), configCodec)
import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Text.IO qualified as TIO
import Error
import Fmt
import Options.Applicative (execParser)
import Polysemy
import Polysemy.Error (Error, errorToIOFinal)
import System.Environment (lookupEnv)
import System.Exit (die, exitFailure)
import Text.Interpolation.Nyan
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
        die $ pretty err

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
          res@VRDirectory{} -> pprintLn $ buildViewResult CLI res
          res@VREntry{} -> pprintLn $ buildViewResult CLI res
          res@VRField{} -> pprintLn $ buildViewResult CLI res
          res@VRPathNotFound{} -> printError $ buildViewResult CLI res
          res@VRDirectoryNoFieldMatch{} -> printError $ buildViewResult CLI res
          res@VREntryNoFieldMatch{} -> printError $ buildViewResult CLI res

      SomeCommand cmd@CmdCreate{} -> do
        runCommand config cmd >>= \case
          res@CRSuccess{} -> printSuccess $ buildCreateResult CLI res
          res@CRCreateError{} -> printError $ buildCreateResult CLI res

      SomeCommand cmd@CmdSetField{} -> do
        runCommand config cmd >>= \case
          res@SFREntryNotFound{} -> printError $ buildSetFieldResult CLI res
          res@SFRSuccess{} -> printSuccess $ buildSetFieldResult CLI res


      SomeCommand cmd@CmdSetFieldVisibility{} -> do
        runCommand config cmd >>= \case
          res@SFVREntryNotFound{} -> printError $ buildSetFieldVisibilityResult CLI res
          res@SFVRSuccess{} -> printSuccess $ buildSetFieldVisibilityResult CLI res
          res@SFVRFieldNotFound{} -> printError $ buildSetFieldVisibilityResult CLI res

      SomeCommand cmd@CmdDeleteField{} -> do
        runCommand config cmd >>= \case
          res@DFREntryNotFound{} -> printError $ buildDeleteFieldResult CLI res
          res@DFRFieldNotFound{} -> printError $ buildDeleteFieldResult CLI res
          res@DFRSuccess{} -> printSuccess $ buildDeleteFieldResult CLI res

      SomeCommand cmd@CmdFind{} -> do
        runCommand config cmd >>= \case
          Just dir -> pprintLn $ buildDirectory dir
          Nothing -> printError "No match found."

      SomeCommand cmd@CmdRename{} -> do
        runCommand config cmd >>= \case
          res@CPRSuccess{} -> pprintLn $ mconcat $ buildRenameResult CLI res
          res@CPRPathNotFound{} -> printError $ mconcat $ buildRenameResult CLI res
          res@CPRMissingEntryName{} -> printError $ mconcat $ buildRenameResult CLI res
          res@CPRCreateErrors{} -> do
            let errorMsgs = buildRenameResult CLI res
            printError [int|s|
              The following entries cannot be renamed:

              #{unlinesF errorMsgs}
            |]
          res@CPRSamePath{} -> printError $ mconcat $ buildRenameResult CLI res

      SomeCommand cmd@CmdCopy{} -> do
        runCommand config cmd >>= \case
          res@CPRSuccess{} -> pprintLn $ mconcat $ buildCopyResult CLI res
          res@CPRPathNotFound{} -> printError $ mconcat $ buildCopyResult CLI res
          res@CPRMissingEntryName{} -> printError $ mconcat $ buildCopyResult CLI res
          res@CPRCreateErrors{} -> do
            let errorMsgs = buildCopyResult CLI res
            printError [int|s|
              The following entries cannot be copied:

              #{unlinesF errorMsgs}
            |]
          res@CPRSamePath{} -> printError $ mconcat $ buildCopyResult CLI res

      SomeCommand cmd@CmdDelete{} -> do
        runCommand config cmd >>= \case
          res@DRPathNotFound{} -> printError $ buildDeleteResult CLI res
          res@DRDirectoryFound{} -> printError $ buildDeleteResult CLI res
          res@DRSuccess{} -> pprintLn $ buildDeleteResult CLI res

      SomeCommand cmd@CmdTag{} -> do
        runCommand config cmd >>= \case
          res@(TREntryNotFound _) -> printError $ buildTagResult CLI res
          res@TRSuccess{} -> printSuccess $ buildTagResult CLI res
          res@TRTagNotFound{} -> printError $ buildTagResult CLI res
          res@TRDuplicateTag{} -> printError $ buildTagResult CLI res
    where
      -- | Pretty-print a message.
      pprintLn :: Member (Embed IO) r => Builder -> Sem r ()
      pprintLn = embed . putStrLn . fmt

      -- | Print a message indicating a successful operation.
      printSuccess :: Member (Embed IO) r => Builder -> Sem r ()
      printSuccess msg = embed $ putStrLn $ "[SUCCESS] " <> fmt msg

      -- | Print a message and exit with `exitFailure`.
      printError :: Member (Embed IO) r => Builder -> Sem r ()
      printError msg = embed $ die $ "[ERROR] " <> fmt msg
