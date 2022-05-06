-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Main where

import Data.Proxy
import Data.Text.IO qualified as TIO
import Network.Wai.Handler.Warp
import Servant.Server

import Backend.Commands
import Config
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.Exit
import Toml qualified
import Web.API
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

main :: IO ()
main = do
  configPath <- getConfigPath
  -- TODO: try sending config via api.
  config <- readConfig configPath
  run 8081 do
    serve (Proxy :: Proxy API) do
      makeServer \_token -> reportErrors . runBackendIO' . runCommand config
