-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backends
  ( SomeBackend (..)
  , someBackendCodec
  , supportedBackendsMap
  , SupportedBackend (..)
  ) where

import Backend (Backend(..))
import Backend.Vault.Kv (VaultKvBackend, kvPathSegmentAllowedCharacters)
import Data.Aeson ((.:))
import Data.Aeson qualified as A
import Data.Aeson.Types (Parser)
import Data.Bifunctor (Bifunctor(first))
import Data.HashMap.Strict qualified as HS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Options.Applicative.Help.Pretty qualified as Pretty
import Servant.API (FromHttpApiData(..))
import Toml (TomlCodec)
import Toml qualified
import Validation (Validation(Failure))

data SomeBackend where
  SomeBackend :: Backend a => a -> SomeBackend

instance Show SomeBackend where
  show (SomeBackend a) = show a

data SupportedBackend = SupportedBackend
  { bType :: String
  , bFromJSON :: A.Value -> Parser SomeBackend
  , bToml :: Toml.TomlEnv SomeBackend
  , bPathHelpMsg :: Pretty.Doc
  }

supportedBackendsMap :: HS.HashMap String SupportedBackend
supportedBackendsMap = HS.fromList [("vault-kv", vaultKVSup)]
  where
    vaultKVSup = SupportedBackend
      "vault-kv"
      ((fmap SomeBackend) . (A.parseJSON @VaultKvBackend))
      (fmap SomeBackend . Toml.codecRead (_codec @VaultKvBackend)) $
      Pretty.vsep
      [ "Vault Kv paths can contain only the following characters:"
      , Pretty.squotes $ Pretty.string kvPathSegmentAllowedCharacters
      ]

instance A.FromJSON SomeBackend where
  parseJSON original = A.withObject "SomeBackend" (\obj ->
      do
        bType :: String <- obj .: "type"
        case (HS.lookup bType supportedBackendsMap) of
           Just supportedBackend -> (bFromJSON supportedBackend) original
           Nothing -> fail $ "Unknown backend type: " <> bType
    ) original

instance FromHttpApiData SomeBackend where
  parseHeader = first T.pack . A.eitherDecodeStrict'
  parseQueryParam t = parseHeader . TE.encodeUtf8 $ t

someBackendCodec :: TomlCodec SomeBackend
someBackendCodec = Toml.Codec input output
  where
    input :: Toml.TomlEnv SomeBackend
    input toml =
      case HS.lookup "type" $ Toml.tomlPairs toml of
        Just t -> do
          case Toml.backward Toml._Text t >>= lookupSupportedBackend of
            Right c -> c toml
            Left e -> Failure [ Toml.BiMapError "type" e ]
        Nothing -> Failure
          [ Toml.BiMapError "type" $ Toml.ArbitraryError
            "Backend doesn't have a `type` key"
          ]
    output (SomeBackend a) = do
      SomeBackend <$> Toml.codecWrite _codec a
        <* Toml.codecWrite (Toml.text "type") "vault"

    lookupSupportedBackend
      :: Text -> Either Toml.TomlBiMapError (Toml.TomlEnv SomeBackend)
    lookupSupportedBackend backend = case HS.lookup (T.unpack backend) supportedBackendsMap of
      Just x -> Right (bToml x)
      Nothing ->  Left (Toml.ArbitraryError "Unknown backend type")
