-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Lens
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Data (Proxy(Proxy))
import Data.OpenApi
import Servant.OpenApi (toOpenApi)
import Web.API (API)

main :: IO ()
main = do
  let openApi = toOpenApi (Proxy :: Proxy API)
        & info . title .~ "Coffer Web API"
        & info . version .~ "1.0"
        & servers .~ ["http://localhost:8081"]
  BL.writeFile "docs/swagger.json" (encodePretty openApi)
