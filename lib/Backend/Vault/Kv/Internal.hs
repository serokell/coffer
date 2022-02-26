{-# LANGUAGE
  TemplateHaskell
, MultiParamTypeClasses
, FlexibleInstances
, FunctionalDependencies
, UndecidableInstances
, BlockArguments
#-}

module Backend.Vault.Kv.Internal
  ( KvResponse (..)
  , requestId, leaseId, renewable, leaseDuration
  , ReadSecret (..)
  , secret, createdTime, deletionTime, destroyed, version
  , ListSecrets (..)
  , unListSecrets
  , PostSecret (..)
  , cas
  , PatchSecret
  , UpdateMetadata (..)
  , maxVersions, casRequired, deleteVersionAfter
  , VaultToken (..)

  , ddata, customMetadata

  , readSecret
  , listSecrets
  , postSecret
  , patchSecret
  , updateMetadata
  , deleteSecret

  , routes
  )
where

import qualified Data.Text              as T;
import qualified Data.HashMap.Strict    as HS;
import qualified Data.Aeson.Types       as AT

import           Data.Aeson
import           Servant.Client.Generic (AsClientT, genericClientHoist)
import           Control.Exception      (throwIO)

import           Control.Lens hiding ((.=))
import           Servant.API
import           Servant.Client
import           Servant.API.Generic

-- |
-- A type defining most responses from the Vault server, it wraps the data we're
-- actually trying to read as some sort of super class.
-- For example:
--
-- >  { "request_id": "00000000-0000-0000-0000-000000000000"
-- >  , "lease_id": ""
-- >  , "renewable": false
-- >  , "lease_duration": 0
-- >  , "data": <DATA>
-- >  , "wrap_info": null
-- >  , "warnings": null
-- >  , "auth": null
-- >  }
data KvResponse a =
  KvResponse
  { krRequestId :: T.Text
  , krLeaseId :: T.Text
  , krRenewable :: Bool
  , krLeaseDuration :: Int
  , krKdata :: a
  }
  deriving stock (Show)
makeLensesWith abbreviatedFields ''KvResponse


-- |
-- A type defining the response from the Vault server at endpoint
-- '/v1/<MOUNT>/metadata/<PATH>'. This is all actually always wrapped
-- in 'KvResponse' at the 'data' key.
-- For example:
--
-- >  { "keys":
-- >    [ "a1"
-- >    , "q1"
-- >    ]
-- >  }
newtype ListSecrets = ListSecrets { _unListSecrets :: [T.Text] }
  deriving stock (Show)
makeLenses ''ListSecrets

-- |
-- A type defining the response from the Vault server at endpoint
-- '/v1/<MOUNT>/data/<PATH>?version=<VERSION>'. This is all actually always wrapped
-- in 'KvResponse' at the 'data' key.
-- For example:
--
-- >  { "data":
-- >    { "token": "stuff"
-- >    }
-- >  , "metadata": <ReadSecretMetadata>
-- >    { "created_time": "2018-03-22T02:24:06.945319214Z",
-- >      "custom_metadata":
-- >      { "owner": "jdoe",
-- >        "mission_critical": "false"
-- >      },
-- >      "deletion_time": "",
-- >      "destroyed": false,
-- >      "version": 2
-- >    }
-- >  }
data ReadSecret =
  ReadSecret
  { rsSecret :: HS.HashMap T.Text T.Text
  , rsCustomMetadata :: HS.HashMap T.Text T.Text
  , rsCreatedTime :: T.Text
  , rsDeletionTime :: T.Text
  , rsDestroyed :: Bool
  , rsVersion :: Int
  }
  deriving stock (Show)
makeLensesWith abbreviatedFields ''ReadSecret


-- |
-- A type defining the request to the Vault servet at endpoint
-- '/v1/<MOUNT>/data/<PATH>'.
-- For example:
--
-- >  { "options":
-- >  , { "cas": 0
-- >    }
-- >  , "data":
-- >    { "foo": "bar"
-- >    , "zip": "zap"
-- >    }
-- >  }
data PostSecret =
  PostSecret
  { psCas :: Maybe Int
  , psDdata :: HS.HashMap T.Text T.Text
  }
  deriving stock (Show)
type PatchSecret = PostSecret
makeLensesWith abbreviatedFields ''PostSecret


-- |
-- A type defining the request to the Vault servet at endpoint
-- '/v1/<MOUNT>/metadata/<PATH>'.
-- For example:
--
-- >  { "max_versions": 5
-- >  , "cas_required": false
-- >  , "delete_version_after": "40m"
-- >  , "custom_metadata":
-- >    { "example": ":)"
-- >    }
-- >  }
data UpdateMetadata =
  UpdateMetadata
  { umMaxVersions :: Maybe Int
  , umCasRequired :: Maybe Bool
  , umDeleteVersionAfter :: Maybe T.Text
  , umCustomMetadata :: HS.HashMap T.Text T.Text
  }
  deriving stock (Show)
makeLensesWith abbreviatedFields ''UpdateMetadata

-- Overloaded Lens accessors

instance {-# OVERLAPPABLE #-} a ~ b => HasDdata (KvResponse a) b where
  ddata = kdata

-- JSON serialization/deserialization, logically some ADTs need to only be deserialized,
-- others serialized, but never both.

instance FromJSON ListSecrets where
  parseJSON = withObject "ListSecrets" $ \o ->
    ListSecrets <$> o .: "keys"

instance FromJSON ReadSecret where
  parseJSON = withObject "ReadSecret" $ \o -> do
    metadata <- o .: "metadata"
    ReadSecret
      <$> o .: "data"
      <*> metadata .:? "custom_metadata" .!= HS.fromList []
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
    <*> o .: "data"

removeNull :: [AT.Pair] -> [AT.Pair]
removeNull = filter \case
  (_, Null) -> False
  _         -> True

instance ToJSON PostSecret where
  toJSON postSecret =
    object
      [ "options" .= object (removeNull [ "cas" .= (postSecret ^. cas)  ])
      , "data" .= (postSecret ^. ddata)
      ]

instance ToJSON UpdateMetadata where
  toJSON updateMetadata =
    object $ removeNull
      [ "max_versions" .= (updateMetadata ^. maxVersions)
      , "cas_required" .= (updateMetadata ^. casRequired)
      , "delete_version_after" .= (updateMetadata ^. deleteVersionAfter)
      , "custom_metadata" .= (updateMetadata ^. customMetadata)
      ]

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
  deriving stock (Eq, Show)

-- Could this be somehow automated? newtypes are just meaningless wrapper anyways, at least to GHC.
instance ToHttpApiData VaultToken where
  toUrlPiece (VaultToken t) = t

-- | The header used for authentication with vault, servant has support for more advanced auth,
--   but since Vault uses tokens and all the other auth methods still eventually lead to a token,
--   this is sufficient.
type VaultTokenHeader = Header' '[Strict, Required] "X-Vault-Token" VaultToken

data Routes route =
  Routes
  { -- | To read a secret under a path use `readSecret`
    rReadSecret :: route
    :- "v1"
    :> Capture "mount" T.Text
    :> "data"
    :> VaultTokenHeader
    :> CaptureAll "segments" T.Text
    :> QueryParam "version" Int
    :> Get '[JSON] (KvResponse ReadSecret)
    -- | To patch a secret under a path use `patchSecret`
  , rPatchSecret :: route
    :- "v1"
    :> Capture "mount" T.Text
    :> "data"
    :> VaultTokenHeader
    :> CaptureAll "segments" T.Text
    :> ReqBody '[JSON] PatchSecret
    :> Patch '[JSON] (KvResponse (HS.HashMap T.Text Value))
    -- | To post a secret to a path use `postSecret`
  , rPostSecret :: route
    :- "v1"
    :> Capture "mount" T.Text
    :> "data"
    :> VaultTokenHeader
    :> CaptureAll "segments" T.Text
    :> ReqBody '[JSON] PostSecret
    :> Post '[JSON] (KvResponse (HS.HashMap T.Text Value))
    -- | To list the paths under a path use `listSecrets`
  , rListSecrets :: route
    :- "v1"
    :> Capture "mount" T.Text
    :> "metadata"
    :> VaultTokenHeader
    :> CaptureAll "segments" T.Text
    :> Verb 'LIST 200 '[JSON] (KvResponse ListSecrets)
    -- | To update metadata under a path use `updateMetadata`
  , rUpdateMetadata :: route
    :- "v1"
    :> Capture "mount" T.Text
    :> "metadata"
    :> VaultTokenHeader
    :> CaptureAll "segments" T.Text
    :> ReqBody '[JSON] UpdateMetadata
    :> Post '[JSON] ()
    -- | To delete secret under a path use `deleteSecret`
  , rDeleteSecret :: route
    :- "v1"
    :> Capture "mount" T.Text
    :> "metadata"
    :> VaultTokenHeader
    :> CaptureAll "segments" T.Text
    :> Delete '[JSON] NoContent
  }
  deriving stock (Generic)
makeLensesWith abbreviatedFields ''Routes

routes :: ClientEnv -> Routes (AsClientT IO)
routes env = genericClientHoist
    (\x -> runClientM x env >>= either throwIO return)
