{-# LANGUAGE TemplateHaskell #-}

module Backend
  ( BackendEffect (..), readSecret, writeSecret, listSecrets
  , Backend (..)
  , BackendPacked (..)
  )
where

import qualified Data.Text           as T
import qualified Entry               as E
import qualified Toml
import qualified Data.HashMap.Strict as HS

import Error                         (CofferError)
import Polysemy.Error                (Error)
import Toml                          (TomlCodec)

import Polysemy

-- @TODO - rename Secret to Entry?
data BackendEffect m a where
  WriteSecret  :: E.Entry -> BackendEffect m Int
  ReadSecret   :: [T.Text] -> Maybe Int -> BackendEffect m E.Entry
  ListSecrets  :: [T.Text] -> BackendEffect m [T.Text]
  DeleteSecret :: [T.Text] -> BackendEffect m ()
makeSem ''BackendEffect

class Show a => Backend a where
  _name :: a -> T.Text
  _codecRead :: Toml.TomlEnv a
  _codecWrite :: a -> Toml.TomlState a
  _runEffect :: Member (Embed IO) r
             => Member (Error CofferError) r
             => a
             -> Sem (BackendEffect ': r) t
             -> Sem r t

data BackendPacked where
  PackBackend :: Backend a => a -> BackendPacked

instance Show BackendPacked where
  show (PackBackend a) = show a
