{-# LANGUAGE TemplateHaskell #-}

module Backend.Vault.Kv
  ( VaultKvBackend
  , runVaultIO
  ) where

import qualified Entry                        as E
import qualified Backend.Vault.Kv.Internal    as I

import           Error                        (CofferError (MarshallingFailed, EntryNotFound, OtherError, ConnectError))
import           Backend                      (Backend (..), BackendEffect (..))

import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy.Encoding      as TL
import qualified Data.Text.Lazy               as TL
import qualified Data.HashMap.Internal.Strict as HS
import qualified Data.Aeson                   as A
import qualified Data.Aeson.Text              as A
import qualified Data.Scientific              as S
import qualified Toml

import           Control.Monad                (void)
import           Servant.Client               (BaseUrl (BaseUrl), Scheme (Https, Http), mkClientEnv, runClientM, ClientError (..), parseBaseUrl, showBaseUrl)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.HTTP.Client          (newManager, defaultManagerSettings)
import           Polysemy.Error               (Error, throw)
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
  { _vbName :: T.Text
  , _vbAddress :: BaseUrl
  , _vbMount :: T.Text
  , _vbToken :: I.VaultToken
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
                  <$> Toml.text "name" Toml..= _vbName
                  <*> didimatch baseUrlToText textToBaseUrl (Toml.text "address") Toml..= _vbAddress
                  <*> Toml.text "mount" Toml..= _vbMount
                  <*> didimatch tokenToText textToToken (Toml.text "token") Toml..= _vbToken
  where tokenToText (I.VaultToken t) = Just t
        textToToken t = Just $ I.VaultToken t
        baseUrlToText = Just . T.pack . showBaseUrl
        textToBaseUrl = parseBaseUrl . T.unpack

data CofferSpecials =
  CofferSpecials
  { _masterKey :: Maybe T.Text
  , _datesModified :: HS.HashMap T.Text UTCTime
  , _privates :: HS.HashMap T.Text Bool
  , _globalDateModified :: UTCTime
  }
  deriving (Show, Generic)
makeLenses ''CofferSpecials

instance A.ToJSON CofferSpecials where
instance A.FromJSON CofferSpecials where

runVaultIO :: Member (Embed IO) r
           => Member (Error CofferError) r
           => BaseUrl
           -> I.VaultToken
           -> T.Text
           -> Sem (BackendEffect ': r) a
           -> Sem r a
runVaultIO url token mount = interpret $
  \x -> do
    env <-
      case url of
        (BaseUrl Http _ _ _) -> do
          manager <- embed $ newManager defaultManagerSettings
          pure $ mkClientEnv manager url
        (BaseUrl Https _ _ _) -> do
          manager <- embed $ newManager tlsManagerSettings
          pure $ mkClientEnv manager url

    case x of
      WriteSecret entry -> do
        let fields = entry ^. E.fields
        let masterField = entry ^. E.masterField
        let datesModified =
              HS.map (^. E.dateModified) . HS.mapKeys E.getFieldKey $ fields
        let privates =
              HS.map (^. E.private) . HS.mapKeys E.getFieldKey $ fields

        let cofferSpecials = CofferSpecials
                             { _masterKey = entry ^. E.masterField <&> E.getFieldKey
                             , _datesModified = datesModified
                             , _globalDateModified = entry ^. E.dateModified
                             , _privates = privates
                             }
        let secret = I.PostSecret
              { I._cas = Nothing
              , I._pdata =
                    HS.insert "#$coffer" (TL.toStrict . TL.decodeUtf8 $ A.encode cofferSpecials)
                  . HS.map (^. E.value)
                  . HS.mapKeys E.getFieldKey
                  $ entry ^. E.fields
              }

        response <- embedCatch (entry ^. E.path) (postSecret env (entry ^. E.path) secret)

        case response ^. I.ddata . at ("version" :: T.Text) of
          Just (A.Number i) -> maybe (throw MarshallingFailed) pure (S.toBoundedInteger i)
          _ -> throw MarshallingFailed
      ReadSecret path version ->
        embedCatch path (readSecret env path version)
        >>= \(I.KvResponse _ _ _ _ (I.ReadSecret _data _ _ _ _ _) _ _ _) -> do
          cofferSpecials :: CofferSpecials <-
            maybe (throw MarshallingFailed) pure
            (_data ^.at "#$coffer" >>= A.decodeStrict' . T.encodeUtf8)
          let secrets = HS.toList $ foldr HS.delete _data ["#$coffer"]
          let keyToField key = do
               modTime <- cofferSpecials ^. datesModified.at key
               _private <- cofferSpecials ^. privates.at key
               value <- _data ^.at key
               key <- E.newFieldKey key

               Just (key, E.newField modTime value & E.private .~ _private)

          fields <- maybe (throw MarshallingFailed) pure $
            over traversed (keyToField . fst) secrets & sequence <&> HS.fromList

          pure $ E.newEntry path (cofferSpecials ^. globalDateModified)
            & E.masterField .~ (cofferSpecials ^. masterKey >>= E.newFieldKey)
            & E.fields .~ fields
      ListSecrets path ->
        embedCatch path (listSecrets env path) <&> (^. I.ddata) <&> \(I.ListSecrets list) -> list
      DeleteSecret path ->
        embedCatch path (deleteSecret env path)
  where postSecret env = (I.routes env ^. I.postSecret) mount token
        readSecret env = (I.routes env ^. I.readSecret) mount token
        listSecrets env = (I.routes env ^. I.listSecrets) mount token
        updateMetadata env = (I.routes env ^. I.updateMetadata) mount token
        deleteSecret env = (I.routes env ^. I.deleteSecret) mount token

        exceptionHandler :: [T.Text] -> ClientError -> CofferError
        exceptionHandler path =
          \case FailureResponse _request response ->
                    case statusCode $ responseStatusCode response of
                      404 -> EntryNotFound path
                      _ -> OtherError
                DecodeFailure text response -> MarshallingFailed
                UnsupportedContentType mediaType response -> MarshallingFailed
                InvalidContentTypeHeader response -> MarshallingFailed
                ConnectionError excepton -> ConnectError
        embedCatch :: Member (Embed IO) r
                       => Member (Error CofferError) r
                       => [T.Text]
                       -> IO a
                       -> Sem r a
        embedCatch path io = embed (catch @ClientError (io <&> Left) (pure . Right . exceptionHandler path)) >>= \case Left l -> pure l ; Right r -> throw r

instance Backend VaultKvBackend where
  _name s = _vbName s
  _codecRead = Toml.codecRead vaultKvCodec
  _codecWrite a = Toml.codecWrite vaultKvCodec a
                  <* Toml.codecWrite (Toml.text "type") "vault"
  _runEffect b = runVaultIO (_vbAddress b) (_vbToken b) (_vbMount b)
