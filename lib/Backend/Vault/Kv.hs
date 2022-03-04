{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Backend.Vault.Kv
  ( VaultKvBackend
  , runVaultIO
  ) where

import qualified Entry                        as E
import qualified Backend.Vault.Kv.Internal    as I

import           Error                        (CofferError (..))
import           Backend                      (Backend (..), BackendEffect (..))

import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy.Encoding      as TL
import qualified Data.Text.Lazy               as TL
import qualified Data.HashMap.Internal.Strict as HS
import qualified Data.Set                     as Set
import qualified Data.Aeson                   as A
import qualified Data.Aeson.Text              as A
import qualified Data.Scientific              as S
import qualified Toml

import           Control.Monad                (void)
import           Servant.Client               (BaseUrl (BaseUrl), Scheme (Https, Http), mkClientEnv, runClientM, ClientError (..), parseBaseUrl, showBaseUrl)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.HTTP.Client          (newManager, defaultManagerSettings)
import           Polysemy.Error               (Error, throw, runError)
import           Control.Exception.Lens       (exception)
import           Control.Monad.State          (evalState)
import           Toml                         (TomlCodec)
import           GHC.Generics                 (Generic)
import           Control.Applicative          (Alternative(empty))
import           Validation                   (Validation(Success, Failure))
import           Data.Time.Format.ISO8601     (iso8601ParseM, iso8601Show)
import           Data.Time                    (UTCTime)
import           Control.Exception            (catch)
import           Servant.Client.Core.Response (responseStatusCode)
import           Servant.Client.Core.Request  (RequestF (Request))
import           Network.HTTP.Types           (statusCode)

import           Polysemy
import           Control.Lens

data VaultKvBackend =
  VaultKvBackend
  { vbName :: T.Text
  , vbAddress :: BaseUrl
  , vbMount :: T.Text
  , vbToken :: I.VaultToken
  }
  deriving (Show)

{-# INLINE didimatch #-}
didimatch
    :: (b -> Maybe a)  -- ^ Mapper for consumer
    -> (a -> Maybe b)     -- ^ Mapper for producer
    -> TomlCodec a  -- ^ Source 'Codec' object
    -> TomlCodec b  -- ^ Target 'Codec' object
didimatch matchB matchA codec = Toml.Codec
    { Toml.codecRead = \t -> case Toml.codecRead codec t of
        Success a -> maybe empty Success (matchA a)
        Failure b -> Failure b
    , Toml.codecWrite = \c -> case matchB c of
        Nothing -> empty
        Just d  -> Toml.codecWrite codec d >>= maybe empty pure . matchA
    }


vaultKvCodec :: TomlCodec VaultKvBackend
vaultKvCodec = VaultKvBackend
                  <$> Toml.text "name" Toml..= vbName
                  <*> didimatch baseUrlToText textToBaseUrl (Toml.text "address") Toml..= vbAddress
                  <*> Toml.text "mount" Toml..= vbMount
                  <*> Toml.dimatch tokenToText textToToken (Toml.text "token") Toml..= vbToken
  where 
    tokenToText (I.VaultToken t) = Just t
    textToToken t = I.VaultToken t
    baseUrlToText = Just . T.pack . showBaseUrl
    textToBaseUrl = parseBaseUrl . T.unpack

data FieldMetadata = FieldMetadata
  { fmDateModified :: UTCTime
  , fmPrivate :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)
makeLensesWith abbreviatedFields ''FieldMetadata

data CofferSpecials =
  CofferSpecials
  { csMasterKey :: Maybe T.Text
  , csGlobalDateModified :: UTCTime
  , csFields :: HS.HashMap T.Text FieldMetadata
  , csTags :: Set.Set T.Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)
makeLensesWith abbreviatedFields ''CofferSpecials

runVaultIO :: Member (Embed IO) r
           => Member (Error CofferError) r
           => BaseUrl
           -> I.VaultToken
           -> T.Text
           -> Sem (BackendEffect ': r) a
           -> Sem r a
runVaultIO url token mount = interpret $
  \operation -> do
    env <-
      case url of
        (BaseUrl Http _ _ _) -> do
          manager <- embed $ newManager defaultManagerSettings
          pure $ mkClientEnv manager url
        (BaseUrl Https _ _ _) -> do
          manager <- embed $ newManager tlsManagerSettings
          pure $ mkClientEnv manager url

    case operation of
      WriteSecret entry -> do
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
                , fmPrivate = f ^. E.private
                })
              & HS.mapKeys E.getFieldKey
            , csTags = Set.map E.getEntryTag $ entry ^. E.tags
            }
          secret = I.PostSecret
            { I.psCas = Nothing
            , I.psDdata =
                    HS.insert "#$coffer" (TL.toStrict $ A.encodeToLazyText cofferSpecials)
                  . HS.map (^. E.value)
                  . HS.mapKeys E.getFieldKey
                  $ entry ^. E.fields
              }

        void $ embedCatch (entry ^. E.path) (postSecret env (entry ^. E.path) secret)
      ReadSecret path ->
        embedCatchMaybe path (readSecret env path Nothing) >>= \case
          Nothing -> pure Nothing
          Just (I.KvResponse _ _ _ _ (I.ReadSecret _data _ _ _ _ _)) -> do
            cofferSpecials :: CofferSpecials <-
              (_data ^.at "#$coffer" >>= A.decodeStrict' . T.encodeUtf8) `orThrow` MarshallingFailed
            let secrets = HS.toList $ HS.delete "#$coffer" _data
            let keyAndValueToField (key, value) = do
                  _modTime <- cofferSpecials ^? fields . at key . _Just . dateModified
                  _private <- cofferSpecials ^? fields . at key . _Just . private
                  _key <- E.newFieldKey key

                  Just (_key
                        , E.newField _modTime value
                          & E.private .~ _private
                        )

            fields <-
              (secrets & each %%~ keyAndValueToField <&> HS.fromList) `orThrow` MarshallingFailed
            _tags <- (cofferSpecials ^. tags & \text -> Set.fromList <$> mapM E.newEntryTag (Set.toList text)) `orThrow` MarshallingFailed

            fieldKey <-
              case cofferSpecials ^. masterKey of
                Nothing -> pure Nothing
                Just mKey ->
                  case E.newFieldKey mKey of
                    Nothing -> throw (OtherError $ "Attempted to create new field key from '" <> mKey <> "'")
                    Just fieldKey -> (pure . Just) fieldKey

            pure . Just
              $ E.newEntry path (cofferSpecials ^. globalDateModified)
              & E.masterField .~ fieldKey
              & E.fields .~ fields
              & E.tags .~ _tags
      ListSecrets path ->
        embedCatchMaybe path $ do
          response <- listSecrets env path
          pure $ response ^. I.ddata . I.unListSecrets
      DeleteSecret path ->
        embedCatch path (void $ deleteSecret env path)

  where 
    postSecret env = (I.routes env ^. I.postSecret) mount token
    readSecret env = (I.routes env ^. I.readSecret) mount token
    listSecrets env = (I.routes env ^. I.listSecrets) mount token
    updateMetadata env = (I.routes env ^. I.updateMetadata) mount token
    deleteSecret env = (I.routes env ^. I.deleteSecret) mount token

    -- | Handles @ClientError@ in the following way: 
    -- 1. If it is @FailureResponse@ and status code isn't 404, then we would get an error. It status code is 404, the result would be Nothing
    -- 2. If it is @ConnectionError@, then we would get @ConnectError@
    -- 3. Otherwise we would get @MarshallingFailed@
    exceptionHandler :: ClientError -> Maybe CofferError
    exceptionHandler =
      \case FailureResponse _request response ->
                case statusCode $ responseStatusCode response of
                  404 -> Nothing
                  e -> Just $ OtherError (T.pack $ show e)
            DecodeFailure text response -> Just MarshallingFailed
            UnsupportedContentType mediaType response -> Just MarshallingFailed
            InvalidContentTypeHeader response -> Just MarshallingFailed
            ConnectionError exception -> Just ConnectError

    -- | Runs an IO action and throws an error if happens.
    embedCatch :: Member (Embed IO) r
                    => Member (Error CofferError) r
                    => [T.Text]
                    -> IO a
                    -> Sem r a
    embedCatch path io = embed (catch @ClientError (io <&> Left) (pure . Right . exceptionHandler)) >>=
      \case Left l -> pure l
            Right (Just r) -> throw r
            Right Nothing -> throw $ OtherError "404"

    -- | Runs an IO action and throws an error only if it isn't a failure response with status code 404.
    --   Otherwise, it would be Nothing.
    embedCatchMaybe :: Member (Embed IO) r
                    => Member (Error CofferError) r
                    => [T.Text]
                    -> IO a
                    -> Sem r (Maybe a)
    embedCatchMaybe path io = embed (catch @ClientError (io <&> Left) (pure . Right . exceptionHandler)) >>=
      \case Left l -> (pure . Just) l
            Right (Just r) -> throw r
            Right Nothing -> pure Nothing

    orThrow :: Member (Error e) r
            => Maybe a
            -> e
            -> Sem r a
    orThrow m e = maybe (throw e) pure m

instance Backend VaultKvBackend where
  _name kvBackend = vbName kvBackend
  _codec = vaultKvCodec
  _runEffect kvBackend = runVaultIO (vbAddress kvBackend) (vbToken kvBackend) (vbMount kvBackend)
