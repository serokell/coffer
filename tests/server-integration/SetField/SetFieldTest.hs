-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module SetField.SetFieldTest where

import Control.Exception (try)
import Control.Lens
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Text (Text)
import Data.Time
import Network.HTTP.Client (responseStatus)
import Network.HTTP.Req
import Network.HTTP.Types (status404, status415)
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

  response <- setField "entry" "private-field" (Just "private") "contents"

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

  response <- setField "entry" "public-field" (Just "public") "contents"

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

-- | Request with no body in set-field must be invalid
-- HTTP Exception with 415 status code (unsupported
-- media type) is expected
unit_set_field_missing_field_contents :: IO ()
unit_set_field_missing_field_contents = cofferTest do
  createEntry "entry"

  result <- try @HttpException
    (executeCommand
      POST
      ["set-field"]
      NoReqBody
      (jsonResponse @Value)
      (mconcat
        [ "path" =: ("/entry" :: Text)
        , "field" =: ("field" :: Text)
        ]
      )
    )

  flip unwrapStatusCodeError result $ \response _ ->
    if responseStatus response == status415
    then pure ()
    else assertFailure $ "Expected Http status 415, got: " <> show response

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

unit_set_field_visibility_public :: IO ()
unit_set_field_visibility_public = cofferTest do
  createEntry "dir/entry"
  setField "dir/entry" "public-field" (Just "private") "contents"
  fieldMb <-
    changeFieldVisibility "dir/entry" "public-field" "public"
      <&> scrubDates
      <&> findField "public-field"

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

unit_set_field_visibility_private :: IO ()
unit_set_field_visibility_private = cofferTest do
  createEntry "dir/entry"
  setField "dir/entry" "private-field" (Just "public") "contents"
  fieldMb <-
    changeFieldVisibility "dir/entry" "private-field" "private"
      <&> scrubDates
      <&> findField "private-field"

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
