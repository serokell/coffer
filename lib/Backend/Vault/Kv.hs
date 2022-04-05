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
import Control.Exception (try)
import Control.Lens hiding ((.=))
import Control.Monad (void)
import Data.Aeson qualified as A
import Data.Aeson.Text qualified as A
import Data.Bifunctor (first)
import Data.Either.Extra (maybeToEither)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HS
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (UTCTime)
import Entry (Entry, Field, FieldContents(FieldContents), FieldName, FieldVisibility)
import Entry qualified as E
import Error (BackendError, CofferError(..))
import Fmt (Buildable(build))
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
import Text.Interpolation.Nyan
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

-- | Errors that can be thrown in Vault backend.
data VaultError
  = ServantError ClientError
  | FieldMetadataNotFound EntryPath FieldName
  | CofferSpecialsNotFound EntryPath
  | BadCofferSpecialsError Text

instance Buildable VaultError where
  build = \case
    ServantError (FailureResponse request response) ->
      [int|s|
        Request:
          #{show request}
        failed with response:
          #{show response}
      |]
    ServantError (DecodeFailure body response) ->
      [int|s|
        The body could not be decoded at the expected type.
        Body: #{body}
        Response:
          #{show response}
      |]
    ServantError (UnsupportedContentType mediatype response) ->
      [int|s|
        The content-type '#{show mediatype}' of the response is not supported.
        Response:
          #{show response}
      |]
    ServantError (InvalidContentTypeHeader response) ->
      [int|s|
        The content-type header is invalid.
        Response:
          #{show response}
      |]
    ServantError (ConnectionError exception) ->
      [int|s|
        Connection error. No response was received.
        #{show exception}
      |]
    FieldMetadataNotFound entryPath fieldName ->
      [int|s|Could not find coffer metadata for field \
      '#{fieldName}' at '#{entryPath}'|]
    CofferSpecialsNotFound entryPath ->
      [int|s|Could not find key '#$coffer' in the kv entry at '#{entryPath}'.|]
    BadCofferSpecialsError err -> build err

instance BackendError VaultError

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
  { csMasterField :: Maybe Text
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

-- | Runs an IO action and throws an error if happens.
embedCatchClientError
  :: Member (Embed IO) r
  => Member (Error CofferError) r
  => IO a
  -> Sem r a
embedCatchClientError io = embed (try @ClientError io) >>= \case
  Left err -> throw $ BackendError (ServantError err)
  Right r -> pure r

-- | Runs an IO action and throws an error only if it isn't a failure response with status code 404.
--   Otherwise, it would be Nothing.
embedCatchClientErrorMaybe
  :: Member (Embed IO) r
  => Member (Error CofferError) r
  => IO a
  -> Sem r (Maybe a)
embedCatchClientErrorMaybe io = embed (try @ClientError io) >>= \case
  Left (FailureResponse _ response)
    | statusCode (responseStatusCode response) == 404 -> pure Nothing
  Left err -> throw $ BackendError (ServantError err)
  Right r -> pure $ Just r

orThrowEither
  :: Member (Error err) r
  => Either e a
  -> (e -> err)
  -> Sem r a
orThrowEither e mkErr = either (throw . mkErr) pure e

getPathSegments
  :: (HasPathSegments s segments, Each segments segments PathSegment PathSegment)
  => s -> [Text]
getPathSegments path = path ^.. pathSegments . each . to unPathSegment

kvWriteSecret :: Effects r => VaultKvBackend -> Entry -> Sem r ()
kvWriteSecret backend entry = do
  let
    cofferSpecials = CofferSpecials
      { csMasterField =
        entry ^. E.masterField <&> E.getFieldName
      , csGlobalDateModified = entry ^. E.dateModified
      , csFields =
        entry ^. E.fields & each %~
          (\f ->
          FieldMetadata
          { fmDateModified = f ^. E.dateModified
          , fmVisibility = f ^. E.visibility
          })
        & HS.mapKeys E.getFieldName
      , csTags = Set.map E.getEntryTag $ entry ^. E.tags
      }
    secret = I.PostSecret
      { I.psCas = Nothing
      , I.psDdata =
            HS.insert "#$coffer" (TL.toStrict $ A.encodeToLazyText cofferSpecials)
          . HS.map (^. E.contents . E.fieldContents)
          . HS.mapKeys E.getFieldName
          $ entry ^. E.fields
        }
  env <- getEnv backend
  void $ embedCatchClientError do
    postSecret env (entry ^. E.path . to getPathSegments) secret
  where
    postSecret env = (I.routes env ^. I.postSecret) (vbMount backend) (vbToken backend)

kvReadSecret :: forall r. Effects r => VaultKvBackend -> EntryPath -> Sem r (Maybe Entry)
kvReadSecret backend path = do
  env <- getEnv backend
  embedCatchClientErrorMaybe (readSecret env (getPathSegments path) Nothing) >>= \case
    Nothing -> pure Nothing
    Just (I.KvResponse _ _ _ _ (I.ReadSecret _data _ _ _ _ _)) -> do
      cofferSpecials <- do
        case _data ^. at "#$coffer" of
          Nothing ->
            throw $ BackendError (CofferSpecialsNotFound path)
          Just txt ->
            txt
              & A.eitherDecodeStrict' @CofferSpecials . T.encodeUtf8
              & first T.pack
              & (`orThrowEither` (BackendError . BadCofferSpecialsError))

      let secrets = HS.toList $ HS.delete "#$coffer" _data

      fields <-
        secrets
          & each %%~ nameAndContentsToField cofferSpecials
          <&> HS.fromList

      _tags <- cofferSpecials ^. tags
            & Set.toList
            & mapM E.newEntryTag
            <&> Set.fromList
            & (`orThrowEither` BadEntryTagError)

      fieldName <-
        case cofferSpecials ^. masterField of
          Nothing -> pure Nothing
          Just mName ->
            case E.newFieldName mName of
              Left err ->
                throw $ BadMasterFieldName mName err
              Right fieldName -> (pure . Just) fieldName

      pure . Just
        $ E.newEntry path (cofferSpecials ^. globalDateModified)
        & E.masterField .~ fieldName
        & E.fields .~ fields
        & E.tags .~ _tags
  where
    readSecret env = (I.routes env ^. I.readSecret) (vbMount backend) (vbToken backend)

    nameAndContentsToField :: CofferSpecials -> (Text, Text) -> Sem r (FieldName, Field)
    nameAndContentsToField cofferSpecials (fieldName, contents) = do
      _fieldName <-
        fieldName
          & E.newFieldName
          & (`orThrowEither` BadFieldNameError)

      metadata <-
        maybe
          (throw $ BackendError (FieldMetadataNotFound path _fieldName))
          pure
          (cofferSpecials ^. fields . at fieldName)

      let _modTime = metadata ^. dateModified
      let _visibility = metadata ^. visibility

      pure (_fieldName
           , E.newField _modTime (FieldContents contents)
              & E.visibility .~ _visibility
           )

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
