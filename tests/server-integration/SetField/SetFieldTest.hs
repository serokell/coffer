-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module SetField.SetFieldTest where

import Control.Exception (try)
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Text (Text)
import Data.Time
import Network.HTTP.Req
import Network.HTTP.Types (status400, status404)
import Test.Tasty.HUnit
import Utils

unit_set_new_field :: IO ()
unit_set_new_field = cofferTest do
  createEntry "entry"

  response <- setField "entry" "newField" "contents"
  response @=
    [aesonQQ|
      {
        "path": "/entry",
        "dateModified": "",
        "masterField": null,
        "fields": {
          "newField": {
            "dateModified": "",
            "visibility": "public",
            "contents": "contents"
          }
        },
        "tags": []
      }
    |]

unit_set_field_private :: IO ()
unit_set_field_private = cofferTest do
  createEntry "entry"
  setField "entry" "public-field" "contents"
  response <-
    executeCommand
      POST
      ["set-field", "private"]
      NoReqBody
      (jsonResponse @Value)
      ( mconcat
          [ "path" =: ("entry" :: Text)
          , "field" =: ("public-field" :: Text)
          ]
      )

  let fieldMb =
        responseBody response
          & scrubDates
          & findField "public-field"

  case fieldMb of
    Nothing -> assertFailure $ "Expected field \"public-field\", found nothing"
    Just value ->
      value @?=
        [aesonQQ|
          {
            "visibility": "private",
            "dateModified": "",
            "contents": "contents"
          }
        |]

unit_set_field_public :: IO ()
unit_set_field_public = cofferTest do
  unit_set_field_private
  response <-
    executeCommand
      POST
      ["set-field", "public"]
      NoReqBody
      (jsonResponse @Value)
      ( mconcat
          [ "path" =: ("entry" :: Text)
          , "field" =: ("public-field" :: Text)
          ]
      )

  let fieldMb =
        responseBody response
          & scrubDates
          & findField "public-field"

  case fieldMb of
    Nothing -> assertFailure $ "Expected field \"public-field\", found nothing"
    Just value ->
      value @?=
        [aesonQQ|
          {
            "visibility": "public",
            "dateModified": "",
            "contents": "contents"
          }
        |]

unit_set_field_entry_not_found :: IO ()
unit_set_field_entry_not_found = cofferTest do
  try @HttpException (setField "entry" "field" "contents") >>=
    processStatusCodeError status404
      [aesonQQ|
        [
          {
            "error": "Entry not found at '/entry'.",
            "code": 300
          }
        ]
      |]

unit_set_field_missing_field_contents :: IO ()
unit_set_field_missing_field_contents = cofferTest do
  createEntry "entry"

  try @HttpException
    (
      executeCommand
        POST
        ["set-field"]
        (ReqBodyJson Null)
        (jsonResponse @Value)
        ( mconcat
            [ "path" =: ("entry" :: Text)
            , "field" =: ("field" :: Text)
            ]
        )
    ) >>=
    processStatusCodeError status400
      [aesonQQ|
        [
          {
            "error": "The entry at '/entry' does not yet have a field 'field'.\nIn order to create a new field, please include 'FIELDCONTENTS' in the body.",
            "code": 301
          }
        ]
      |]

unit_set_field_updates_modification_date :: IO ()
unit_set_field_updates_modification_date = cofferTest do
  createEntry "entry"
  setField "entry" "field" "contents"

  t1 <- getCurrentTime
  response <- setField "entry" "field" "contents2"
  t2 <- getCurrentTime

  let newModifiedDate = responseBody response ^?! key "dateModified" . _JSON @_ @UTCTime
  assertBool "Date is not recent"
    $ t1 <= newModifiedDate && newModifiedDate <= t2

  assertBool "Dates are not equal"
    $ responseBody response ^?! key "fields" . key "field" . key "dateModified" . _JSON == newModifiedDate
