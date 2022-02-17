module Error
  ( VaultError (..)
  , CofferError (..)
  ) where

import qualified Data.Text         as T

import           Servant.Client    (BaseUrl)
import           Control.Exception (SomeException)

data VaultError =
  ConnectionFailed SomeException BaseUrl
  | ParseResponseFailed T.Text
  | EntryNotFound [T.Text]
  deriving (Show)

data CofferError =
  Vault VaultError
  | MarshallingFailed
  deriving (Show)
