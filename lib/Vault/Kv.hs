{-# LANGUAGE TemplateHaskell #-}

module Vault.Kv
  (Vault (..)
  , runVaultIO
  ) where

import qualified Entry                        as E
import qualified Vault.Kv.Internal            as I

import           Error                        (CofferError (MarshallingFailed), VaultError (ConnectionFailed))

import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Lazy.Encoding      as TL
import qualified Data.Text.Lazy               as TL
import qualified Data.HashMap.Internal.Strict as HS
import qualified Data.Aeson                   as A
import qualified Data.Aeson.Text              as A
import qualified Data.Scientific              as S

import           Control.Monad                (void)
import           Servant.Client               (BaseUrl (BaseUrl), Scheme (Https, Http), mkClientEnv, runClientM, ClientError (..))
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.HTTP.Client          (newManager, defaultManagerSettings)
import           Polysemy.Error               (Error, throw)
import           Control.Exception.Lens       (exception)
import           Control.Monad.State          (evalState)

import           Polysemy
import           Control.Lens
import GHC.Generics (Generic)

data Vault m a where
  WriteSecret :: E.Entry -> Vault m Int
  ReadSecret  :: [T.Text] -> Maybe Int -> Vault m E.Entry
  ListSecrets :: [T.Text] -> Vault m [T.Text]
makeSem ''Vault

data CofferSpecials =
  CofferSpecials
  { _masterKey :: T.Text
  , _datesModified :: HS.HashMap T.Text T.Text
  , _globalDateModified :: T.Text
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
           -> Sem (Vault ': r) a
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
        let datesModified =
              HS.mapKeys E.getFieldKey $
              HS.insert (masterField ^. _1) (masterField ^. _2) fields
              & traverse %~ (^. E.dateModified)
              where fields = entry ^. E.fields
                    masterField = entry ^. E.masterField
        let cofferSpecials = CofferSpecials
                             { _masterKey = entry ^. E.masterField . _1 & E.getFieldKey
                             , _datesModified = datesModified
                             , _globalDateModified = entry ^. E.dateModified
                             }
        let secret = I.PostSecret
              { I._cas = Nothing
              , I._pdata =
                  HS.insert "#$coffer" (TL.toStrict . TL.decodeUtf8 $ A.encode cofferSpecials) $
                  HS.mapKeys E.getFieldKey $
                  HS.insert (masterField ^. _1) (masterField ^. _2) fields
                  & traverse %~ (^. E.value)
              }
              where fields = entry ^. E.fields
                    masterField = entry ^. E.masterField

        response <- embed (postSecret env (entry ^. E.path) secret)
        embed $ print cofferSpecials

        case response ^. I.ddata . at ("version" :: T.Text) of
          Just (A.Number i) -> maybe (throw MarshallingFailed) pure (S.toBoundedInteger i)
          _ -> throw MarshallingFailed
      ReadSecret path version ->
        embed (readSecret env path version)
        >>= \(I.KvResponse _ _ _ _ (I.ReadSecret _data _ _ _ _ _) _ _ _) -> do
          cofferSpecials :: CofferSpecials <-
            maybe (throw MarshallingFailed) pure
            (_data ^.at "#$coffer" >>= A.decodeStrict' . T.encodeUtf8)
          let secrets = HS.toList $ foldr HS.delete _data ["#$coffer", cofferSpecials ^. masterKey]
          let keyToField key = do
               modTime <- cofferSpecials ^.datesModified.at key
               value <- _data ^.at key
               key <- E.newFieldKey key

               Just (key, E.Field { E._fDateModified = modTime, E._value = value })

          fields <- maybe (throw MarshallingFailed) pure $
            over traversed (keyToField . fst) secrets & sequence <&> HS.fromList

          embed $ print fields
          maybe (throw MarshallingFailed) pure $ do
            master <- keyToField (cofferSpecials ^. masterKey)
            Just E.Entry
              { E._path = path
              , E._eDateModified = cofferSpecials ^. globalDateModified
              , E._masterField = master
              , E._fields = fields
              }
      ListSecrets path ->
        embed (listSecrets env path) <&> (^. I.ddata) <&> \(I.ListSecrets list) -> list

  where postSecret env = (I.routes env ^. I.postSecret) mount token
        readSecret env = (I.routes env ^. I.readSecret) mount token
        listSecrets env = (I.routes env ^. I.listSecrets) mount token
        updateMetadata env = (I.routes env ^. I.updateMetadata) mount token
