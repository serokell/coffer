-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Config where

import Backend (Backend(..))
import BackendName (BackendName, backendNameCodec)
import Backends (SomeBackend(..), someBackendCodec)
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HS
import Toml (TomlCodec, (.=))
import Toml qualified

data Config =
  Config
  { backends :: HashMap BackendName SomeBackend
  , mainBackend :: BackendName
  }
  deriving stock (Show)

makeSingleBackendConfig :: SomeBackend -> Config
makeSingleBackendConfig (SomeBackend backend) = Config (HS.singleton (_name backend) (SomeBackend backend)) (_name backend)

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.dimap toList listToHs
  (Toml.list someBackendCodec "backend") .= backends
  <*> backendNameCodec "main_backend" .= mainBackend
  where
    listToHs list = HS.fromList $ fmap (\y@(SomeBackend x) -> (_name x, y)) list
