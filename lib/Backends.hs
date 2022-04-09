-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backends
  ( supportedBackends
  , backendPackedCodec
  ) where

import Backend (Backend(..), SomeBackend(..))
import Backend.Vault.Kv (VaultKvBackend)
import Data.HashMap.Strict qualified as HS
import Data.Text (Text)
import Toml (TomlCodec)
import Toml qualified
import Validation (Validation(Failure))

backendPackedCodec :: TomlCodec SomeBackend
backendPackedCodec = Toml.Codec input output
  where
    input :: Toml.TomlEnv SomeBackend
    input toml =
      case HS.lookup "type" $ Toml.tomlPairs toml of
        Just t -> do
          case Toml.backward Toml._Text t >>= supportedBackends of
            Right c -> c toml
            Left e -> Failure [ Toml.BiMapError "type" e ]
        Nothing -> Failure
          [ Toml.BiMapError "type" $ Toml.ArbitraryError
            "Backend doesn't have a `type` key"
          ]
    output (SomeBackend a) = do
      SomeBackend <$> Toml.codecWrite _codec a
        <* Toml.codecWrite (Toml.text "type") "vault"

supportedBackends
  :: Text -> Either Toml.TomlBiMapError (Toml.TomlEnv SomeBackend)
supportedBackends "vault-kv" = Right $ fmap SomeBackend . Toml.codecRead (_codec @VaultKvBackend)
supportedBackends _ = Left (Toml.ArbitraryError "Unknown backend type")
