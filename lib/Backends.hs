module Backends
  ( supportedBackends
  , backendPackedCodec
  ) where

import qualified Data.Text           as T
import qualified Toml

import           Backend.Vault.Kv    (VaultKvBackend)
import           Toml                (TomlCodec)
import           Backend             (BackendPacked (..), Backend (..))
import qualified Data.HashMap.Strict as HS
import Validation (Validation(Failure))

backendPackedCodec
  :: (T.Text -> Either Toml.TomlBiMapError (Toml.TomlEnv BackendPacked))
  -> TomlCodec BackendPacked
backendPackedCodec backends = Toml.Codec input output
  where input :: Toml.TomlEnv BackendPacked
        input toml = case HS.lookup "type" $ Toml.tomlPairs toml of
                       Just t -> do
                         case Toml.backward Toml._Text t >>= backends of
                           Right c -> c toml
                           Left e -> Failure
                                     [ Toml.BiMapError "type" e
                                     ]
                       Nothing -> Failure
                                  [ Toml.BiMapError "type" $ Toml.ArbitraryError
                                    "Backend doesn't have a `type` key"
                                  ]
        output (PackBackend a) =  PackBackend <$> _codecWrite a

supportedBackends
  :: T.Text -> Either Toml.TomlBiMapError (Toml.TomlEnv BackendPacked)
supportedBackends "vault-kv" = Right $ fmap PackBackend . _codecRead @VaultKvBackend
supportedBackends _ = Left (Toml.ArbitraryError "Unknow backend type")
