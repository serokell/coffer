-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backends
  ( supportedBackends
  ) where

import Backend (Backend(..), SomeBackend(..))
import Backend.Pass
import Backend.Vault.Kv (VaultKvBackend)
import Data.Text (Text)
import Toml qualified

supportedBackends
  :: Text -> Either Toml.TomlBiMapError (Toml.TomlEnv SomeBackend)
supportedBackends "vault-kv" = Right $ fmap SomeBackend . Toml.codecRead (_codec @VaultKvBackend)
supportedBackends "pass" = Right $ fmap SomeBackend . Toml.codecRead (_codec @PassBackend)
supportedBackends _ = Left (Toml.ArbitraryError "Unknown backend type")
