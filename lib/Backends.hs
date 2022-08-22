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
import Backend.Vault.Kv (VaultKvBackend(..), VaultToken(..), kvPathSegmentAllowedCharacters)
import BackendName (newBackendName)
import Control.Exception
import Control.Lens
import Data.Aeson (toJSON, (.:))
import Data.Aeson qualified as A
import Data.Aeson.Lens
import Data.Aeson.Types (Parser)
import Data.Bifunctor (Bifunctor(first))
import Data.Data (Proxy(..))
import Data.HashMap.Strict qualified as HS
import Data.OpenApi (Referenced(..), Schema, ToParamSchema(..), toSchema)
import Data.OpenApi.Lens as Schema
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Options.Applicative.Help.Pretty qualified as Pretty
import Servant.API (FromHttpApiData(..))
import Servant.Client (parseBaseUrl)
import Toml (TomlCodec)
import Toml qualified
import Validation (Validation(Failure))

data SomeBackend where
  SomeBackend :: Backend a => a -> SomeBackend

data SupportedBackend = SupportedBackend
  { bType :: String
  , bFromJSON :: A.Value -> Parser SomeBackend
  , bToml :: Toml.TomlEnv SomeBackend
  , bPathHelpMsg :: Pretty.Doc
  , bSchema :: Schema
  }

makeLensesFor [("bSchema", "supportedBackendSchema")] ''SupportedBackend

supportedBackendsMap :: HS.HashMap String SupportedBackend
supportedBackendsMap = addTypeKeyToSchema <$> HS.fromList [("vault-kv", vaultKVSup)]
  where
    vaultKVSup = SupportedBackend
      { bType = "vault-kv"
      , bFromJSON = fmap SomeBackend . A.parseJSON @VaultKvBackend
      , bToml = fmap SomeBackend . Toml.codecRead (_codec @VaultKvBackend)
      , bPathHelpMsg = Pretty.vsep
          [ "Vault Kv paths can contain only the following characters:"
          , Pretty.squotes $ Pretty.string kvPathSegmentAllowedCharacters
          ]
      , bSchema =
          toSchema @VaultKvBackend Proxy
      }
    -- Add the "type" key to each Schema.
    addTypeKeyToSchema :: SupportedBackend -> SupportedBackend
    addTypeKeyToSchema sb@SupportedBackend{bType} =
      sb
        & supportedBackendSchema . properties . at "type" ?~ Inline
            (toSchema @Text Proxy & enum_ ?~ [fromString bType])

instance Show SomeBackend where
  show (SomeBackend a) = show a

instance ToParamSchema SomeBackend where
  toParamSchema _ =
    mempty
      & oneOf ?~ (Inline . bSchema <$> HS.elems supportedBackendsMap)
      & example ?~ exampleBackend
    where
      -- Use the "vault-kv" config as an example.
      exampleBackend :: A.Value
      exampleBackend =
        toJSON VaultKvBackend
          { vbName = either (error . T.unpack) id $ newBackendName "vault-local"
          , vbAddress = either (error . displayException) id $ parseBaseUrl "http://localhost:8200"
          , vbMount = "secret"
          , vbToken = VaultToken "root"
          }
          & _Object . at "type" ?~ "vault-kv"

instance A.FromJSON SomeBackend where
  parseJSON original = A.withObject "SomeBackend" (\obj ->
      do
        bType :: String <- obj .: "type"
        case HS.lookup bType supportedBackendsMap of
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
