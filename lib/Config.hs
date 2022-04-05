-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Config where

import Backend (Backend(..), SomeBackend(..))
import Backend.Debug
import BackendName (BackendName, backendNameCodec)
import Backends (supportedBackends)
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HS
import Data.Text (Text)
import Toml (TomlCodec, (.=))
import Toml qualified
import Validation

data Config =
  Config
  { backends :: HashMap BackendName SomeBackend
  , mainBackend :: BackendName
  }
  deriving stock (Show)

backendPackedCodec :: TomlCodec SomeBackend
backendPackedCodec = Toml.Codec input output
  where
    input :: Toml.TomlEnv SomeBackend
    input toml =
      case HS.lookup "type" $ Toml.tomlPairs toml of
        Just t -> do
          case Toml.backward Toml._Text t >>= supportedBackendsWithDebug of
            Right c -> c toml
            Left e -> Failure [ Toml.BiMapError "type" e ]
        Nothing -> Failure
          [ Toml.BiMapError "type" $ Toml.ArbitraryError
            "Backend doesn't have a `type` key"
          ]
    output (SomeBackend a) = do
      SomeBackend <$> Toml.codecWrite _codec a
        <* Toml.codecWrite (Toml.text "type") "vault"

supportedBackendsWithDebug
  :: Text -> Either Toml.TomlBiMapError (Toml.TomlEnv SomeBackend)
supportedBackendsWithDebug "debug" = Right $ fmap SomeBackend . Toml.codecRead (_codec @DebugBackend)
supportedBackendsWithDebug t = supportedBackends t

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.dimap toList listToHs
  (Toml.list backendPackedCodec "backend") .= backends
  <*> backendNameCodec "main_backend" .= mainBackend
  where
    listToHs list = HS.fromList $ fmap (\y@(SomeBackend x) -> (_name x, y)) list
