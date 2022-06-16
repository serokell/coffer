-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module DeleteField.DeleteFieldTest
  ( unit_delete_field
  , unit_delete_field_entry_not_found
  , unit_delete_field_field_not_found
  , unit_delete_field_updates_entry's_modification_time
  ) where

import Control.Exception (try)
import Control.Lens
import Control.Monad (void)
import Control.Monad.Extra (whenJust)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Text (Text)
import Data.Time
import Network.HTTP.Req
import Network.HTTP.Types (status404)
import Test.Tasty.HUnit
import Utils

executeDeleteField :: Text -> Text -> IO (JsonResponse Value)
executeDeleteField path field =
  executeCommand
    DELETE
    ["delete-field"]
    NoReqBody
    (jsonResponse @Value)
    ( mconcat
        [ "path" =: path
        , "field" =: field
        ]
    )

unit_delete_field :: IO ()
unit_delete_field = cofferTest do
  createEntry "entry"
  void $ setField "entry" "field" "contents"

  response <- executeDeleteField "entry" "field"

  let value = responseBody response

  whenJust (findField "field" value) \_ -> do
    assertFailure "Field \"field\" not deleted"

unit_delete_field_entry_not_found :: IO ()
unit_delete_field_entry_not_found = cofferTest do
  try @HttpException (executeDeleteField "notexists" "field") >>=
    processStatusCodeError status404
      [aesonQQ|
        [
          {
            "error": "Entry not found at '/notexists'.",
            "code": 400
          }
        ]
      |]

unit_delete_field_field_not_found :: IO ()
unit_delete_field_field_not_found = cofferTest do
  createEntry "entry"

  try @HttpException (executeDeleteField "entry" "notexists") >>=
    processStatusCodeError status404
      [aesonQQ|
        [
          {
            "error": "Entry does not have a field with name 'notexists'.",
            "code": 401
          }
        ]
      |]

unit_delete_field_updates_entry's_modification_time :: IO ()
unit_delete_field_updates_entry's_modification_time = cofferTest do
  createEntry "entry"
  response <- setField "entry" "field" "contents"

  let oldModificationTime = responseBody response ^?! key "dateModified"

  t1 <- getCurrentTime
  response <- executeDeleteField "entry" "field"
  t2 <- getCurrentTime

  let updatedEntry = responseBody response
  let newTime = updatedEntry ^?! key "dateModified" . _JSON

  assertBool "Date is not recent"
    $ t1 <= newTime && newTime <= t2

  assertBool "Dates are equal"
    $ updatedEntry ^?! key "dateModified" /= oldModificationTime
