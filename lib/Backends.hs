-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0
{-# OPTIONS_GHC -Wno-orphans #-}

module Backends
  ( supportedBackends
  , backendPackedCodec
  ) where

import Backend (Backend(..), SomeBackend(..))
import Backend.Vault.Kv (VaultKvBackend)
import Data.Aeson ((.:))
import Data.Aeson qualified as A
import Data.HashMap.Strict qualified as HS
import Data.Text (Text)
import Toml (TomlCodec)
import Toml qualified
import Validation (Validation(Failure))

instance A.FromJSON SomeBackend where
  parseJSON original = A.withObject "SomeBackend" (\obj ->
    do
      bType :: String <- obj .: "type"
      case bType of
        "vault-kv" -> fmap SomeBackend $ A.parseJSON @VaultKvBackend original
        _ -> fail "Unknown backend type") original

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
