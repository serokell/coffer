-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Vault.Common
  ( unit_vault_incorrect_path_segment_fromHttpApiData
  , unit_vault_incorrect_backend_name
  ) where

import Control.Exception
import Data.Aeson (encode)
import Data.Aeson qualified as A
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Network.HTTP.Client (Response(..))
import Network.HTTP.Req
import Network.HTTP.Types (status400, status500)
import Test.Tasty.HUnit ((@?=))
import Utils (cofferTest, executeCommandWithHeader, processStatusCodeError, unwrapStatusCodeError)

unit_vault_incorrect_backend_name :: IO ()
unit_vault_incorrect_backend_name = cofferTest do
  try @HttpException
    (executeCommandWithHeader
        errHeader
        POST
        ["create"]
        (ReqBodyJson emptyNewEntry)
        ignoreResponse
        ("path" =: ("entry" :: Text))
      ) >>= unwrapStatusCodeError \response bs -> do
        bs @?= "Error parsing header Coffer-Backend failed: Error in $.name: Backend name can only contain the following characters: 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_;'"
        responseStatus response @?= status400
    where
    errHeader = LBS.toStrict . encode $
      [aesonQQ|
          {
            "type" : "vault-kv",
            "name" : "vault-local#",
            "address" : "localhost:8212",
            "mount" : "secret",
            "token" : "root"
          }
      |]

unit_vault_incorrect_path_segment_fromHttpApiData :: IO ()
unit_vault_incorrect_path_segment_fromHttpApiData = cofferTest do

  try @HttpException
    (executeCommandWithHeader
        header
        POST
        ["create"]
        (ReqBodyJson emptyNewEntry)
        ignoreResponse
        ("path" =: ("entry:." :: Text)))
    >>= processStatusCodeError status500
      [aesonQQ|
        [
          {
            "error" : "Invalid path segment for target backend:\nGot: \"entry:.\".\n\nPath segments for Vault KV can only contain the following characters:\n'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_'.",
            "code"  : 0
          }
        ]
      |]
  where
    header = LBS.toStrict . encode $
      [aesonQQ|
          {
            "type" : "vault-kv",
            "name" : "vault-local",
            "address" : "localhost:8212",
            "mount" : "secret",
            "token" : "root"
          }
      |]

emptyNewEntry :: A.Value
emptyNewEntry =
      [aesonQQ|
        {
          "fields": { },
          "tags": []
        }
      |]
