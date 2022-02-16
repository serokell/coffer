{-# LANGUAGE
  TemplateHaskell
, MultiParamTypeClasses
, FlexibleInstances
#-}

module Backend.Vault.Kv.Internal
  ( KvResponse (..)
  , requestId, leaseId, renewable, leaseDuration, wrapInfo, warnings, auth
  , ReadSecret (..)
  , secret, createdTime, deletionTime, destroyed, version
  , ListSecrets (..)
  , PostSecret (..)
  , cas
  , PatchSecret (..)
  , UpdateMetadata (..)
  , maxVersions, casRequired, deleteVersionAfter
  , VaultToken (..)

  , ddata, customMetadata

  , readSecret
  , listSecrets
  , postSecret
  , patchSecret
  , updateMetadata

  , routes
  )
where

import qualified Data.Text              as T;
import qualified Data.HashMap.Strict    as HS;
import qualified Data.Aeson             as A
import qualified Data.Aeson.Types       as AT

import           Data.Aeson             ((.:)) -- Control.Lens shadows (.=), use (A..=) instead
import           Control.Applicative    ((<|>))
import           Data.Proxy             (Proxy (Proxy))
import           Servant.Client.Generic (AsClientT, genericClientHoist, genericClient)
import           Control.Exception      (throwIO)

import           Control.Lens
import           Servant.API
import           Servant.Client
import           Servant.API.Generic

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
  , _leaseDuration :: Int
  , _kdata :: a
  , _wrapInfo :: Maybe ()
  , _warnings :: Maybe ()
  , _auth :: Maybe ()
  }
  deriving (Show)
makeLenses ''KvResponse


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
makeLenses ''ListSecrets

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
  , _rCustomMetadata :: HS.HashMap T.Text T.Text
  , _createdTime :: T.Text
  , _deletionTime :: T.Text
  , _destroyed :: Bool
  , _version :: Int
  }
  deriving (Show)
makeLenses ''ReadSecret


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
  { _cas :: Maybe Int
  , _pdata :: HS.HashMap T.Text T.Text
  }
  deriving (Show)
type PatchSecret = PostSecret
makeLenses ''PostSecret


-- |
-- A type defining the request to the Vault servet at endpoint
-- '/v1/<MOUNT>/metadata/<PATH>'.
-- For example:
--
-- { "max_versions": 5
-- , "cas_required": false
-- , "delete_version_after": "40m"
-- , "custom_metadata":
--   { "example": ":)"
--   }
-- }
data UpdateMetadata =
  UpdateMetadata
  { _maxVersions :: Maybe Int
  , _casRequired :: Maybe Bool
  , _deleteVersionAfter :: Maybe T.Text
  , _uCustomMetadata :: HS.HashMap T.Text T.Text
  }
  deriving (Show)
makeLenses ''UpdateMetadata

-- Overloaded Lens accessors

class DataLens a b where
  ddata :: Lens' a b

instance a ~ b => DataLens (KvResponse a) b where
  ddata = kdata

instance DataLens PostSecret (HS.HashMap T.Text T.Text) where
  ddata = pdata

class CustomMetadataLens a where
  customMetadata :: Lens' a (HS.HashMap T.Text T.Text)

instance CustomMetadataLens ReadSecret where
  customMetadata = rCustomMetadata

instance CustomMetadataLens UpdateMetadata where
  customMetadata = uCustomMetadata

-- JSON serialization/deserialization, logically some ADTs need to only be deserialized,
-- others serialized, but never both.

instance A.FromJSON ListSecrets where
  parseJSON = A.withObject "ListSecrets" $ \o ->
    ListSecrets <$> o .: "keys"

instance A.FromJSON ReadSecret where
  parseJSON = A.withObject "ReadSecret" $ \o -> do
    metadata <- o .: "metadata"
    ReadSecret
      <$> o .: "data"
      <*> (metadata .: "custom_metadata" <|> pure (HS.fromList []))
      <*> metadata .: "created_time"
      <*> metadata .: "deletion_time"
      <*> metadata .: "destroyed"
      <*> metadata .: "version"

instance A.FromJSON a => A.FromJSON (KvResponse a) where
  parseJSON = A.withObject "KvResponse" $ \o ->
    KvResponse
    <$> o .: "request_id"
    <*> o .: "lease_id"
    <*> o .: "renewable"
    <*> o .: "lease_duration"
    <*> (o .: "data" >>= A.parseJSON)
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing

removeNull :: [AT.Pair] -> [AT.Pair]
removeNull = filter (\(_, v) -> case v of A.Null -> False ; _ -> True)

instance A.ToJSON PostSecret where
  toJSON postSecret =
    A.object [ "options" A..= A.object (removeNull [ "cas" A..= (postSecret ^. cas)  ])
             , "data" A..= ((postSecret ^. ddata) :: HS.HashMap T.Text T.Text)
             ]

instance A.ToJSON UpdateMetadata where
  toJSON updateMetadata =
    A.object (removeNull [ "max_versions" A..= (updateMetadata ^. maxVersions)
                         , "cas_required" A..= (updateMetadata ^. casRequired)
                         , "delete_version_after" A..= (updateMetadata ^. deleteVersionAfter)
                         , "custom_metadata" A..= (updateMetadata ^. uCustomMetadata)
                         ])

-- Then we can declare the finalized API.


-- | Vault uses a non-standard LIST method, this code was dug up from `servant` and its dependencies.
--   I don't remember how I figured it out, it just works.
data ExtraMethods =
  LIST

instance ReflectMethod 'LIST where
  reflectMethod _ = "LIST"


-- TODO - A place holder, for a perhaps more complicated type. One with pinned memory which is
--        overwritten many times or something like that
newtype VaultToken = VaultToken T.Text
  deriving (Eq, Show)

-- Could this be somehow automated? newtypes are just meaningless wrapper anyways, at least to GHC.
instance ToHttpApiData VaultToken where
  toUrlPiece (VaultToken t) = t

-- | The header used for authentication with vault, servant has support for more advanced auth,
--   but since Vault uses tokens and all the other auth methods still eventually lead to a token,
--   this is sufficient.
type VaultTokenHeader = Header' '[Strict, Required] "X-Vault-Token" VaultToken

-- $setup
-- >>> import Network.HTTP.Client.TLS (tlsManagerSettings)
-- >>> import Network.HTTP.Client     (newManager)
-- >>> import Servant.Clieng          (BaseUrl, Https)
--
-- >>> let vaultToken   = "TOKEN"
-- >>> let vaultAddress = "vault.example.org"
--
-- >>>     manager     <- newManager tlsManagerSettings
-- >>> let clientEnv   <- mkClientEnv manager (BaseUrl Https vaultAddress 8200 "")


data Routes route =
  Routes
  { -- | To read a secret under a path use `readSecret`
    -- >>> (routes clientEnv ^. readSecret) vaultToken "kv" ["testbed", "a1"]
    _readSecret :: route
    :- "v1"
    :> Capture "mount" T.Text
    :> "data"
    :> VaultTokenHeader
    :> CaptureAll "segments" T.Text
    :> QueryParam "version" Int
    :> Get '[JSON] (KvResponse ReadSecret)
    -- | To patch a secret under a path use `patchSecret`
    -- >>> (routes clientEnv ^. patchSecret) vaultToken "kv" ["testbed", "a1"] secret
  , _patchSecret :: route
    :- "v1"
    :> Capture "mount" T.Text
    :> "data"
    :> VaultTokenHeader
    :> CaptureAll "segments" T.Text
    :> ReqBody '[JSON] PatchSecret
    :> Patch '[JSON] (KvResponse (HS.HashMap T.Text A.Value))
    -- | To post a secret to a path use `postSecret`
    -- >>> (routes clientEnv ^. postSecret) vaultToken "kv" ["testbed", "a1"] secret
  , _postSecret :: route
    :- "v1"
    :> Capture "mount" T.Text
    :> "data"
    :> VaultTokenHeader
    :> CaptureAll "segments" T.Text
    :> ReqBody '[JSON] PostSecret
    :> Post '[JSON] (KvResponse (HS.HashMap T.Text A.Value))
    -- | To list the paths under a path use `listSecrets`
    -- >>> (routes clientEnv ^. listSecrets) vaultToken "kv" ["testbed"]
  , _listSecrets :: route
    :- "v1"
    :> Capture "mount" T.Text
    :> "metadata"
    :> VaultTokenHeader
    :> CaptureAll "segments" T.Text
    :> Verb 'LIST 200 '[JSON] (KvResponse ListSecrets)
    -- | To update metadata under a path use `updateMetadata`
    -- >>> (routes clientEnv ^. updateMetadata) vaultToken "kv" ["testbed"] metadata
  , _updateMetadata :: route
    :- "v1"
    :> Capture "mount" T.Text
    :> "metadata"
    :> VaultTokenHeader
    :> CaptureAll "segments" T.Text
    :> ReqBody '[JSON] UpdateMetadata
    :> Post '[JSON] ()
  }
  deriving (Generic)
makeLenses ''Routes

routes :: ClientEnv -> Routes (AsClientT IO)
routes env = genericClientHoist
    (\x -> runClientM x env >>= either throwIO return)
