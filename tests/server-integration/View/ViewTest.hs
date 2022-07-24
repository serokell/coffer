-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module View.ViewTest where

import Control.Exception (try)
import Control.Monad (void)
import Data.Aeson (Value)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Text (Text)
import Network.HTTP.Req
import Network.HTTP.Types (status404)
import Utils

unit_view_an_entry :: IO ()
unit_view_an_entry = cofferTest do
  createEntry "dir/entry"
  void $ setField "dir/entry" "public-field" Nothing "contents"
  void $ setField "dir/entry" "private-field" Nothing "multiline\ncontents"
  changeFieldVisibility "dir/entry" "private-field" False

  response <-
    executeCommand
      GET
      ["view"]
      NoReqBody
      (jsonResponse @Value)
      ("path" =: ("dir/entry" :: Text))

  response @=
    [aesonQQ|
      {
        "entries": [],
        "subdirs": {
          "dir": {
            "entries": [
              {
                "path": "/dir/entry",
                "dateModified": "",
                "masterField": null,
                "fields": {
                  "public-field": {
                    "dateModified": "",
                    "visibility": "public",
                    "contents": "contents"
                  },
                  "private-field": {
                    "dateModified": "",
                    "visibility": "private",
                    "contents": "multiline\ncontents"
                  }
                },
                "tags": []
              }
            ],
            "subdirs": {}
          }
        }
      }
    |]

unit_view_a_directory :: IO ()
unit_view_a_directory = cofferTest do
  createEntry "dir/a"
  createEntry "dir/b"
  createEntry "dir/c"

  response <-
    executeCommand
      GET
      ["view"]
      NoReqBody
      (jsonResponse @Value)
      ("path" =: ("dir" :: Text))

  response @=
    [aesonQQ|
      {
        "entries": [],
        "subdirs": {
          "dir": {
            "entries": [
              {
                "path": "/dir/a",
                "dateModified": "",
                "masterField": null,
                "fields": {},
                "tags": []
              },
              {
                "path": "/dir/b",
                "dateModified": "",
                "masterField": null,
                "fields": {},
                "tags": []
              },
              {
                "path": "/dir/c",
                "dateModified": "",
                "masterField": null,
                "fields": {},
                "tags": []
              }
            ],
            "subdirs": {}
          }
        }
      }
    |]

unit_view_path_not_found :: IO ()
unit_view_path_not_found = cofferTest do
  try @HttpException viewRoot >>=
    processStatusCodeError status404
      [aesonQQ|
        [
          {
            "error": "Entry or directory not found at '/'.",
            "code": 100
          }
        ]
      |]
