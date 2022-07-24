-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module SetField.SetFieldTest where

import Control.Exception (try)
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time
import Network.HTTP.Req
import Network.HTTP.Types (status404)
import Test.Tasty.HUnit
import Utils

unit_set_new_field :: IO ()
unit_set_new_field = cofferTest do
  createEntry "entry"

  response <- setField "entry" "newField" Nothing "contents"
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

  response <- setField "entry" "private-field" (Just False) "contents"

  let fieldMb =
        responseBody response
          & scrubDates
          & findField "private-field"

  case fieldMb of
    Nothing -> assertFailure $ "Expected field \"private-field\", found nothing"
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
  createEntry "entry"

  response <- setField "entry" "public-field" (Just True) "contents"

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
  try @HttpException (setField "entry" "field" Nothing "contents") >>=
    processStatusCodeError status404
      [aesonQQ|
        [
          {
            "error": "Entry not found at '/entry'.",
            "code": 300
          }
        ]
      |]

-- unit_set_field_missing_field_contents :: IO ()
-- TODO: add test that expects an error in case of missing contents

unit_set_field_updates_modification_date :: IO ()
unit_set_field_updates_modification_date = cofferTest do
  createEntry "entry"
  setField "entry" "field" Nothing "contents"

  t1 <- getCurrentTime
  response <- setField "entry" "field" Nothing "contents2"
  t2 <- getCurrentTime

  let newModifiedDate = responseBody response ^?! key "dateModified" . _JSON @_ @UTCTime
  assertBool "Date is not recent"
    $ t1 <= newModifiedDate && newModifiedDate <= t2

  assertBool "Dates are not equal"
    $ responseBody response ^?! key "fields" . key "field" . key "dateModified" . _JSON == newModifiedDate
