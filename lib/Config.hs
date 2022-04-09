-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Config where

import Backend (Backend(..), SomeBackend(..))
import BackendName (BackendName, backendNameCodec)
import Backends (backendPackedCodec)
import Data.Foldable (toList)
import Data.HashMap.Strict qualified as HS
import Toml (TomlCodec)
import Toml qualified

data Config =
  Config
  { backends :: HS.HashMap BackendName SomeBackend
  , mainBackend :: BackendName
  }
  deriving stock (Show)


configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.dimap toList listToHs
  (Toml.list backendPackedCodec "backend") Toml..= backends
  <*> backendNameCodec "main_backend" Toml..= mainBackend
  where
    listToHs list = HS.fromList $ fmap (\y@(SomeBackend x) -> (_name x, y)) list
