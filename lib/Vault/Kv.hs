{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Vault.Kv
  ( readSecret
  , VaultToken (..)
  ) where

import Vault.SecretEngine ( SecretEngine )

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HS
import Control.Lens
import GHC.Generics (Generic)

import Servant.API
import Servant.Client
import GHC.Base (Symbol)
import Data.Proxy (Proxy (Proxy))
import Data.Aeson ( FromJSON(parseJSON), fromJSON, ToJSON, (.:), Value (Object), withObject, toJSON, object )
import qualified Data.Aeson as A ((.=))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Client (newManager)
import Control.Applicative (Alternative((<|>)))


-- |
-- A type defining most responses from the Vault server, it wraps the data we're
-- actually trying to read as some sort of super class.
-- For example:
--
-- { "request_id": "00000000-0000-0000-0000-000000000000"
-- , "lease_id": ""
-- , "renewable": false
-- , "lease_duration": 0
-- , "data": <DATA>
-- , "wrap_info": null
-- , "warnings": null
-- , "auth": null
-- }
data KvResponse a =
  KvResponse
  { _requestId :: T.Text
  , _leaseId :: T.Text
  , _renewable :: Bool
  , _lease_duration :: Int
  , _kdata :: a
  , _wrapInfo :: Maybe ()
  , _warnings :: Maybe ()
  , _auth :: Maybe ()
  }
  deriving (Show)


-- |
-- A type defining the response from the Vault server at endpoint
-- '/v1/<MOUNT>/metadata/<PATH>'. This is all actually always wrapped
-- in 'KvResponse' at the 'data' key.
-- For example:
--
-- { "keys":
--   [ "a1"
--   , "q1"
--   ]
-- }
newtype ListSecrets = ListSecrets [T.Text]
  deriving (Show)


-- |
-- A type defining the response from the Vault server at endpoint
-- '/v1/<MOUNT>/data/<PATH>?version=<VERSION>'. This is all actually always wrapped
-- in 'KvResponse' at the 'data' key.
-- For example:
--
-- { "data":
--   { "token": "stuff"
--   }
-- , "metadata": <ReadSecretMetadata>
--   { "created_time": "2018-03-22T02:24:06.945319214Z",
--     "custom_metadata":
--     { "owner": "jdoe",
--       "mission_critical": "false"
--     },
--     "deletion_time": "",
--     "destroyed": false,
--     "version": 2
--   }
-- }
data ReadSecret =
  ReadSecret
  { _secret :: HS.HashMap T.Text T.Text
  , _customMetadata :: HS.HashMap T.Text T.Text
  , _createdTime :: T.Text
  , _deletionTime :: T.Text
  , _destroyed :: Bool
  , _version :: Int
  }
  deriving (Show)


-- |
-- A type defining the request to the Vault servet at endpoint
-- '/v1/<MOUNT>/data/<PATH>'.
-- For example:
--
-- { "options":
-- , { "cas": 0
--   }
-- , "data":
--   { "foo": "bar"
--   , "zip": "zap"
--   }
-- }
data PostSecret =
  PostSecret
  { _cas :: Int
  , _pdata :: HS.HashMap T.Text T.Text
  }
  deriving (Show)
type PatchSecret = PostSecret

instance FromJSON ListSecrets where
  parseJSON = withObject "ListSecrets" $ \o ->
    ListSecrets <$> o .: "keys"

instance FromJSON ReadSecret where
  parseJSON = withObject "ReadSecret" $ \o -> do
    metadata <- o .: "metadata"
    ReadSecret
      <$> o .: "data"
      <*> (metadata .: "custom_metadata" <|> pure (HS.fromList []))
      <*> metadata .: "created_time"
      <*> metadata .: "deletion_time"
      <*> metadata .: "destroyed"
      <*> metadata .: "version"

instance FromJSON a => FromJSON (KvResponse a) where
  parseJSON = withObject "KvResponse" $ \o ->
    KvResponse
    <$> o .: "request_id"
    <*> o .: "lease_id"
    <*> o .: "renewable"
    <*> o .: "lease_duration"
    <*> (o .: "data" >>= parseJSON)
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing

instance ToJSON PostSecret where
  toJSON PostSecret { _cas = _cas, _pdata = _pdata } =
    object [ "options" A..= object [ "cas" A..= _cas  ]
           , "data" A..= _pdata
           ]

makeLenses ''ReadSecret
makeLenses ''ListSecrets
makeLenses ''PostSecret

newtype VaultToken = VaultToken T.Text
  deriving (Eq, Show)

type VaultTokenHeader = Header' '[Strict, Required] "X-Vault-Token" VaultToken

instance ToHttpApiData VaultToken where
  toUrlPiece (VaultToken t) = t

data ExtraMethods =
  LIST

instance ReflectMethod 'LIST where
  reflectMethod _ = "LIST"

type KvAPI =
         "data"
         :> VaultTokenHeader
         :> CaptureAll "segments" T.Text
         :> QueryParam "version" Int
         :> Get '[JSON] (KvResponse ReadSecret)
       :<|> "data"
         :> VaultTokenHeader
         :> CaptureAll "segments" T.Text
         :> ReqBody '[JSON] PatchSecret
         :> Patch '[JSON] (KvResponse ())
       :<|> "data"
         :> VaultTokenHeader
         :> CaptureAll "segments" T.Text
         :> ReqBody '[JSON] PostSecret
         :> Post '[JSON] ()
       :<|> "metadata"
         :> VaultTokenHeader
         :> CaptureAll "segments" T.Text
         :> Verb 'LIST 200 '[JSON] (KvResponse ListSecrets)

api :: Proxy ("v1" :> "kv" :> KvAPI)
api = Proxy

-- $setup
-- >>> import System.Environment (getEnv)
-- >>> import qualified Data.Text as T
-- >>> import Network.HTTP.Client.TLS (tlsManagerSettings)
-- >>> import Network.HTTP.Client (newManager)
-- >>> import Servant.API
-- >>> import Servant.Client
--
-- >>> vaultToken <- getEnv "VAULT_TOKEN" >>= pure . VaultToken . T.pack
-- >>> vaultAddress <- getEnv "VAULT_ADDRESS"
-- >>> manager <- newManager tlsManagerSettings
-- >>> let clientEnv =  mkClientEnv manager (BaseUrl Https vaultAddress 8200 "")

-- |
-- To read a secret under a path use 'readSecret'
--
-- >>> runClientM (readSecret vaultToken ["testbed/a1"]) clientEnv
-- ...
--
-- To list secrets under a path, you use 'listSecrets'
--
-- >>> runClientM (listSecrets vaultToken ["testbed"]) clientEnv
-- ...
--
readSecret :<|>
  patchSecret :<|>
  postSecret :<|>
  listSecrets
  = client api

clientEnv :: IO ClientEnv
clientEnv = do
  manager <- newManager tlsManagerSettings
  return $ mkClientEnv manager (BaseUrl Https "vault.in.redalder.org" 8200 "")

data Secret =
  Secret
  { _primary :: T.Text
  , _auxiliaries :: HS.HashMap T.Text T.Text
  }
  deriving (Generic, Show)

makeLenses ''Secret
