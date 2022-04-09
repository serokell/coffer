-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backend.Vault.Kv
  ( VaultKvBackend
  , I.VaultToken(..)
  ) where

import Backend (Backend(..), Effects)
import Backend.Vault.Kv.Internal qualified as I
import BackendName (BackendName, backendNameCodec)
import Coffer.Path (EntryPath, HasPathSegments, Path, PathSegment, pathSegments, unPathSegment)
import Coffer.Util (didimatch)
import Control.Exception (catch)
import Control.Lens hiding ((.=))
import Control.Monad (void)
import Data.Aeson qualified as A
import Data.Aeson.Text qualified as A
import Data.Either.Extra (eitherToMaybe, maybeToEither)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime)
import Entry (Entry, FieldValue(FieldValue), FieldVisibility)
import Entry qualified as E
import Error (CofferError(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode)
import Polysemy
import Polysemy.Error (Error, throw)
import Servant.Client
  (BaseUrl(BaseUrl), ClientEnv, ClientError(..), Scheme(Http, Https), mkClientEnv, parseBaseUrl,
  showBaseUrl)
import Servant.Client.Core.Response (responseStatusCode)
import Toml (TomlCodec, (.=))
import Toml qualified

data VaultKvBackend =
  VaultKvBackend
  { vbName :: BackendName
  , vbAddress :: BaseUrl
  , vbMount :: Text
  , vbToken :: I.VaultToken
  }
  deriving stock (Show)

vaultKvCodec :: TomlCodec VaultKvBackend
vaultKvCodec = VaultKvBackend
  <$> backendNameCodec "name" .= vbName
  <*> didimatch baseUrlToText textToBaseUrl (Toml.text "address") .= vbAddress
  <*> Toml.text "mount" .= vbMount
  <*> Toml.dimatch tokenToText textToToken (Toml.text "token") .= vbToken
  where
    tokenToText (I.VaultToken t) = Just t
    textToToken t = I.VaultToken t

    baseUrlToText :: BaseUrl -> Either Text Text
    baseUrlToText = Right . T.pack . showBaseUrl

    textToBaseUrl :: Text -> Either Text BaseUrl
    textToBaseUrl = maybeToEither "Cannot parse base url" . parseBaseUrl . T.unpack

data FieldMetadata = FieldMetadata
  { fmDateModified :: UTCTime
  , fmVisibility :: FieldVisibility
  }
  deriving stock (Show, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)
makeLensesWith abbreviatedFields ''FieldMetadata

data CofferSpecials =
  CofferSpecials
  { csMasterKey :: Maybe Text
  , csGlobalDateModified :: UTCTime
  , csFields :: HashMap Text FieldMetadata
  , csTags :: Set Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)
makeLensesWith abbreviatedFields ''CofferSpecials

getEnv :: Effects r => VaultKvBackend -> Sem r ClientEnv
getEnv backend =
  case url of
    (BaseUrl Http _ _ _) -> do
      manager <- embed $ newManager defaultManagerSettings
      pure $ mkClientEnv manager url
    (BaseUrl Https _ _ _) -> do
      manager <- embed $ newManager tlsManagerSettings
      pure $ mkClientEnv manager url
  where
    url = vbAddress backend

-- | Handles @ClientError@ in the following way:
-- 1. If it is @FailureResponse@ and status code isn't 404, then we would get an error.
--    It status code is 404, the result would be Nothing
-- 2. If it is @ConnectionError@, then we would get @ConnectError@
-- 3. Otherwise we would get @MarshallingFailed@
exceptionHandler :: ClientError -> Maybe CofferError
exceptionHandler = \case
  FailureResponse _request response ->
    case statusCode $ responseStatusCode response of
      404 -> Nothing
      e -> Just $ OtherError (T.pack $ show e)
  DecodeFailure _ _ -> Just MarshallingFailed
  UnsupportedContentType _ _ -> Just MarshallingFailed
  InvalidContentTypeHeader _ -> Just MarshallingFailed
  ConnectionError _ -> Just ConnectError

-- | Runs an IO action and throws an error if happens.
embedCatchClientError
  :: Member (Embed IO) r
  => Member (Error CofferError) r
  => IO a
  -> Sem r a
embedCatchClientError io =
  embed (catch @ClientError (io <&> Left) (pure . Right . exceptionHandler)) >>= \case
    Left l -> pure l
    Right (Just r) -> throw r
    Right Nothing -> throw $ OtherError "404"

-- | Runs an IO action and throws an error only if it isn't a failure response with status code 404.
--   Otherwise, it would be Nothing.
embedCatchClientErrorMaybe
  :: Member (Embed IO) r
  => Member (Error CofferError) r
  => IO a
  -> Sem r (Maybe a)
embedCatchClientErrorMaybe io =
  embed (catch @ClientError (io <&> Left) (pure . Right . exceptionHandler)) >>= \case
    Left l -> (pure . Just) l
    Right (Just r) -> throw r
    Right Nothing -> pure Nothing

orThrow
  :: Member (Error e) r
  => Maybe a
  -> e
  -> Sem r a
orThrow m e = maybe (throw e) pure m

getPathSegments
  :: (HasPathSegments s segments, Each segments segments PathSegment PathSegment)
  => s -> [Text]
getPathSegments path = path ^.. pathSegments . each . to unPathSegment

kvWriteSecret :: Effects r => VaultKvBackend -> Entry -> Sem r ()
kvWriteSecret backend entry = do
  let
    cofferSpecials = CofferSpecials
      { csMasterKey =
        entry ^. E.masterField <&> E.getFieldKey
      , csGlobalDateModified = entry ^. E.dateModified
      , csFields =
        entry ^. E.fields & each %~
          (\f ->
          FieldMetadata
          { fmDateModified = f ^. E.dateModified
          , fmVisibility = f ^. E.visibility
          })
        & HS.mapKeys E.getFieldKey
      , csTags = Set.map E.getEntryTag $ entry ^. E.tags
      }
    secret = I.PostSecret
      { I.psCas = Nothing
      , I.psDdata =
            HS.insert "#$coffer" (TL.toStrict $ A.encodeToLazyText cofferSpecials)
          . HS.map (^. E.value . E.fieldValue)
          . HS.mapKeys E.getFieldKey
          $ entry ^. E.fields
        }
  env <- getEnv backend
  void $ embedCatchClientError do
    postSecret env (entry ^. E.path . to getPathSegments) secret
  where
    postSecret env = (I.routes env ^. I.postSecret) (vbMount backend) (vbToken backend)

kvReadSecret :: Effects r => VaultKvBackend -> EntryPath -> Sem r (Maybe Entry)
kvReadSecret backend path = do
  env <- getEnv backend
  embedCatchClientErrorMaybe (readSecret env (getPathSegments path) Nothing) >>= \case
    Nothing -> pure Nothing
    Just (I.KvResponse _ _ _ _ (I.ReadSecret _data _ _ _ _ _)) -> do
      cofferSpecials :: CofferSpecials <-
        (_data ^.at "#$coffer" >>= A.decodeStrict' . T.encodeUtf8) `orThrow` MarshallingFailed
      let secrets = HS.toList $ HS.delete "#$coffer" _data
      let keyAndValueToField (key, value) = do
            _modTime <- cofferSpecials ^? fields . at key . _Just . dateModified
            _visibility <- cofferSpecials ^? fields . at key . _Just . visibility
            _key <- eitherToMaybe $ E.newFieldKey key

            Just
              (_key
              , E.newField _modTime (FieldValue value)
                & E.visibility .~ _visibility
              )

      fields <-
        (secrets & each %%~ keyAndValueToField <&> HS.fromList) `orThrow` MarshallingFailed
      _tags <- cofferSpecials ^. tags
        & Set.toList
        & mapM E.newEntryTag
        <&> Set.fromList
        & eitherToMaybe
        & (`orThrow` MarshallingFailed)

      fieldKey <-
        case cofferSpecials ^. masterKey of
          Nothing -> pure Nothing
          Just mKey ->
            case eitherToMaybe $ E.newFieldKey mKey of
              Nothing -> throw (OtherError $ "Attempted to create new field key from '" <> mKey <> "'")
              Just fieldKey -> (pure . Just) fieldKey

      pure . Just
        $ E.newEntry path (cofferSpecials ^. globalDateModified)
        & E.masterField .~ fieldKey
        & E.fields .~ fields
        & E.tags .~ _tags
  where
    readSecret env = (I.routes env ^. I.readSecret) (vbMount backend) (vbToken backend)

kvListSecrets :: Effects r => VaultKvBackend -> Path -> Sem r (Maybe [Text])
kvListSecrets backend path = do
  env <- getEnv backend
  embedCatchClientErrorMaybe do
    response <- listSecrets env (getPathSegments path)
    pure $ response ^. I.ddata . I.unListSecrets
  where
    listSecrets env = (I.routes env ^. I.listSecrets) (vbMount backend) (vbToken backend)

kvDeleteSecret :: Effects r => VaultKvBackend -> EntryPath -> Sem r ()
kvDeleteSecret backend path = do
  env <- getEnv backend
  embedCatchClientError (void $ deleteSecret env (getPathSegments path))
  where
    deleteSecret env = (I.routes env ^. I.deleteSecret) (vbMount backend) (vbToken backend)

instance Backend VaultKvBackend where
  _name kvBackend = vbName kvBackend
  _codec = vaultKvCodec
  _writeSecret = kvWriteSecret
  _readSecret = kvReadSecret
  _listSecrets = kvListSecrets
  _deleteSecret = kvDeleteSecret
