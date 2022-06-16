-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Create.CreateTest
  ( unit_create_an_entry
  , unit_create_with_force
  , unit_create_parent_directory_is_an_entry
  , unit_create_destination_is_directory
  , unit_create_entry_already_exists
  , unit_create_set_same_dates_for_fields_and_entry
  ) where

import Control.Exception (try)
import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Text (Text)
import Data.Time
import Network.HTTP.Req
import Network.HTTP.Types (status409)
import Test.Tasty.HUnit
import Utils

executeCreateCommand :: Text -> Bool -> Value -> IO (JsonResponse Value)
executeCreateCommand path force entry =
  executeCommand
    POST
    ["create"]
    (ReqBodyJson entry)
    (jsonResponse @Value)
    ( mconcat
        [ "path" =: path
        , if force then queryFlag "force" else mempty
        ]
    )

emptyNewEntry :: Value
emptyNewEntry =
  [aesonQQ|
    {
      "fields": {},
      "tags": []
    }
  |]

newEntryWithFields :: Value
newEntryWithFields =
  [aesonQQ|
    {
      "fields":
        {
          "public-field": { "contents": "field1", "visibility": "public" },
          "private-field": { "contents": "multiline\nfield", "visibility": "private"}
        },
      "tags": ["first-tag", "second-tag"]
    }
  |]

unit_create_an_entry :: IO ()
unit_create_an_entry = cofferTest do
  response <- executeCreateCommand "entry" False newEntryWithFields
  response @=
    [aesonQQ|
      {
        "path": "/entry",
        "dateModified": "",
        "masterField": null,
        "fields": {
          "public-field": {
            "dateModified": "",
            "visibility": "public",
            "contents": "field1"
          },
          "private-field": {
            "dateModified": "",
            "visibility": "private",
            "contents": "multiline\nfield"
          }
        },
        "tags": [
          "first-tag",
          "second-tag"
        ]
      }
    |]

unit_create_with_force :: IO ()
unit_create_with_force = cofferTest do
  createEntry "clashed"

  response <- executeCreateCommand "clashed" True emptyNewEntry
  response @=
    [aesonQQ|
      {
        "path": "/clashed",
        "dateModified": "",
        "masterField": null,
        "fields": {},
        "tags": []
      }
    |]

unit_create_parent_directory_is_an_entry :: IO ()
unit_create_parent_directory_is_an_entry = cofferTest do
  createEntry "a/b"

  try @HttpException (executeCreateCommand "a/b/c" False emptyNewEntry) >>=
    processStatusCodeError status409
      [aesonQQ|
        [
          {
            "error": "Attempted to create the directory '/a/b' but an entry exists at that path.",
            "code": 200
          }
        ]
      |]

unit_create_destination_is_directory :: IO ()
unit_create_destination_is_directory = cofferTest do
  createEntry "a/b/c"

  try @HttpException (executeCreateCommand "a/b" False emptyNewEntry) >>=
    processStatusCodeError status409
      [aesonQQ|
        [
          {
            "error": "'/a/b' is a directory.",
            "code": 201
          }
        ]
      |]

unit_create_entry_already_exists :: IO ()
unit_create_entry_already_exists = cofferTest do
  createEntry "a"

  try @HttpException (executeCreateCommand "a" False emptyNewEntry) >>=
    processStatusCodeError status409
      [aesonQQ|
        [
          {
            "error": "An entry already exists at '/a'.\nUse 'force' query flag to overwrite existing entries.",
            "code": 202
          }
        ]
      |]

unit_create_set_same_dates_for_fields_and_entry :: IO ()
unit_create_set_same_dates_for_fields_and_entry = cofferTest do
  t1 <- getCurrentTime
  response <- executeCreateCommand "entry" False newEntryWithFields
  t2 <- getCurrentTime

  let entry = responseBody response
  let time = entry ^?! key "dateModified" . _JSON

  assertBool "Date is not recent"
    $ t1 <= time && time <= t2

  assertBool "Dates are not the same"
    $ entry ^?! key "fields" . key "public-field" . key "dateModified" . _JSON == time

  assertBool "Dates are not the same"
    $ entry ^?! key "fields" . key "private-field" . key "dateModified" . _JSON == time
