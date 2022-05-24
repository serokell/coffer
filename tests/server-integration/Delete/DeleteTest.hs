-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Delete.DeleteTest
  ( unit_delete_entry
  , unit_delete_directory
  , unit_delete_path_not_found
  , unit_delete_directory_found
  ) where

import Control.Exception (try)
import Data.Aeson.QQ.Simple (aesonQQ)
import Network.HTTP.Req
import Network.HTTP.Types (status400, status404)
import Test.Tasty.HUnit
import Utils

unit_delete_entry :: IO ()
unit_delete_entry = cofferTest do
  createEntry "toDelete"

  response <- executeDeleteCommand "toDelete" False
  responseStatusCode response @?= 204

unit_delete_directory :: IO ()
unit_delete_directory = cofferTest do
  createEntry "a/b"
  createEntry "a/c"
  createEntry "a/d"

  response <- executeDeleteCommand "a" True
  responseStatusCode response @?= 204

  viewResponse <- try @HttpException viewRoot
  case viewResponse of
    Left _ -> pure ()
    Right res -> assertFailure $ "Expected directory to be deleted, got " <> show res

unit_delete_path_not_found :: IO ()
unit_delete_path_not_found = cofferTest do
  try @HttpException (executeDeleteCommand "notexists" False) >>=
    processStatusCodeError status404
      [aesonQQ|
        [
          {
            "error": "Entry or directory not found at '/notexists'.",
            "code": 600
          }
        ]
      |]

unit_delete_directory_found :: IO ()
unit_delete_directory_found = cofferTest do
  createEntry "a/b"

  try @HttpException (executeDeleteCommand "a" False) >>=
    processStatusCodeError status400
      [aesonQQ|
        [
          {
            "error": "The path '/a' is a directory.\nUse 'recursive' query flag to recursively delete all entries.",
            "code": 601
          }
        ]
      |]
