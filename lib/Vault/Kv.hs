{-# LANGUAGE TemplateHaskell #-}

module Vault.Kv
  () where

import qualified Entry                   as E
import qualified Vault.Kv.Internal       as I

import qualified Data.Text               as T

import           Servant.Client          (BaseUrl (BaseUrl), Scheme (Https, Http), mkClientEnv, runClientM)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Client     (newManager, defaultManagerSettings)

import           Polysemy
import           Control.Lens
import Control.Monad (void)

data Vault m a where
  WriteSecret :: E.Entry -> Vault m ()
  ReadSecret  :: [T.Text] -> Vault m E.Entry
  ListSecrets :: [T.Text] -> Vault m [T.Text]
makeSem ''Vault

runVaultIO :: Member (Embed IO) r
           => BaseUrl
           -> I.VaultToken
           -> T.Text
           -> Sem (Vault ': r) a
           -> Sem r a
runVaultIO url@(BaseUrl Http _ _ _) token mount = interpret $ \x -> do
  manager <- embed $ newManager tlsManagerSettings
  let env = mkClientEnv manager url

  case x of
    WriteSecret entry -> embed . void $ runClientM (I.postSecret token mount (entry ^. E.path) undefined) env
    ReadSecret  path  -> embed $ runClientM (I.readSecret token mount path undefined) env >>= const undefined
    ListSecrets path  -> embed $ runClientM (I.listSecrets token mount path) env >>= const undefined

runVaultIO url@(BaseUrl Https _ _ _) token mount = interpret $ \x -> do
  manager <- embed $ newManager defaultManagerSettings
  let env = mkClientEnv manager url

  case x of
    WriteSecret entry -> embed . void $ runClientM (I.postSecret token mount (entry ^. E.path) undefined) env
    ReadSecret  path  -> embed $ runClientM (I.readSecret token mount path undefined) env >>= const undefined
    ListSecrets path  -> embed $ runClientM (I.listSecrets token mount path) env >>= const undefined
