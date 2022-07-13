-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Web.Main
  ( runServer
  ) where

import Backend.Commands (runCommand)
import Config
import Data.Proxy
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import Web.API (API)
import Web.Server

runServer :: IO ()
runServer = do
  run 8081 do
    serve (Proxy :: Proxy API) do
      makeServer \someBackend -> reportErrors . runBackendIO' . runCommand (makeSingleBackendConfig  someBackend)
