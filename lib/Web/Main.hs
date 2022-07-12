-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Web.Main
  ( runServer
  ) where

import Backend.Commands (runCommand)
import Config
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Toml qualified
import Web.API (API)
import Web.Server

readConfig :: FilePath -> IO Config
readConfig configPath = do
  text <- TIO.readFile configPath
  let config = Toml.decode configCodec text
  case config of
    Left err -> do
      TIO.putStrLn $ "An error occurred while decoding '" <> T.pack configPath <> "':"
      TIO.putStrLn $ Toml.prettyTomlDecodeErrors err
      exitFailure
    Right config -> pure config

getConfigPath :: IO FilePath
getConfigPath = do
  envConfigPathMb <- lookupEnv "COFFER_CONFIG"
  pure $ fromMaybe "config.toml" envConfigPathMb

runServer :: IO ()
runServer = do
  configPath <- getConfigPath
  -- TODO: try sending config via api.
  config <- readConfig configPath
  run 8081 do
    serve (Proxy :: Proxy API) do
      makeServer \someBackend -> reportErrors . runBackendIO' . runCommand (makeSingleBackendConfig  someBackend)
