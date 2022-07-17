-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Common.Common
  ( unit_incorrect_backend_name
  , unit_incorrect_field_name_fromJSONKey
  , unit_incorrect_field_name_fromHttpApiData
  , unit_incorrect_tag_name_fromJSON
  , unit_incorrect_tag_name_fromHttpApiData
  , unit_incorrect_path_segment_fromHttpApiData
  ) where

import Control.Exception
import Data.Aeson (encode)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.ByteString.Lazy qualified as LBS
import Data.Text
import Network.HTTP.Client (Response(responseStatus))
import Network.HTTP.Req
import Network.HTTP.Types (status400)
import Test.Tasty.HUnit
import Utils

unit_incorrect_backend_name :: IO ()
unit_incorrect_backend_name = cofferTest do
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
    emptyNewEntry =
      [aesonQQ|
        {
          "fields": { },
          "tags": []
        }
      |]
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

unit_incorrect_field_name_fromJSONKey :: IO()
unit_incorrect_field_name_fromJSONKey = cofferTest do
  try @HttpException
    (executeCommand
        POST
        ["create"]
        (ReqBodyJson emptyNewEntry)
        ignoreResponse
        ("path" =: ("entry" :: Text))
      ) >>= unwrapStatusCodeError \response bs -> do
        bs @?= "Error in $.fields['fieldname:.']: Field name can only contain the following characters: 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_;'"
        responseStatus response @?= status400
    where
    emptyNewEntry =
      [aesonQQ|
        {
          "fields":
            {
              "fieldname:." :
                {
                  "contents" : "contents",
                  "visibility" : "public"
                }
            },
          "tags": []
        }
      |]

unit_incorrect_field_name_fromHttpApiData :: IO()
unit_incorrect_field_name_fromHttpApiData = cofferTest do
  createEntry "entry"

  try @HttpException ( setField "entry" "fieldname:." "contents" )
    >>= unwrapStatusCodeError \response bs -> do
      bs @?= "Error parsing query parameter field failed: Field name can only contain the following characters: 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_;'"
      responseStatus response @?= status400

unit_incorrect_tag_name_fromJSON :: IO()
unit_incorrect_tag_name_fromJSON = cofferTest do
  try @HttpException
    (executeCommand
        POST
        ["create"]
        (ReqBodyJson emptyNewEntry)
        ignoreResponse
        ("path" =: ("entry" :: Text))
      ) >>= unwrapStatusCodeError \response bs -> do
        bs @?= "Error in $.tags[0]: Tags can only contain the following characters: 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_;'"
        responseStatus response @?= status400
    where
    emptyNewEntry =
      [aesonQQ|
        {
          "fields": {},
          "tags": [ "tag:." ]
        }
      |]

unit_incorrect_tag_name_fromHttpApiData :: IO ()
unit_incorrect_tag_name_fromHttpApiData = cofferTest do
  createEntry "tagged"

  try @HttpException ( addOrRemoveTag POST "tagged" "tag:." )
    >>= unwrapStatusCodeError \response bs -> do
      bs @?= "Error parsing query parameter tag failed: Tags can only contain the following characters: 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_;'"
      responseStatus response @?= status400

unit_incorrect_path_segment_fromHttpApiData :: IO ()
unit_incorrect_path_segment_fromHttpApiData = cofferTest do

  try @HttpException ( createEntry "entry:." )
    >>= unwrapStatusCodeError \response bs -> do
      bs @?= "Error parsing query parameter path failed: Path segments can only contain the following characters: 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_'"
      responseStatus response @?= status400
