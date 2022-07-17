-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Tag.TagTest
  ( unit_tag_entry
  , unit_delete_tag
  , unit_tag_entry_not_found
  , unit_tag_tag_not_found
  , unit_tag_duplicate_tag
  , unit_tag_updates_only_entry's_modification_time
  ) where

import Control.Exception (try)
import Control.Lens
import Control.Monad (void)
import Data.Aeson.Lens
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Time
import Network.HTTP.Req
import Network.HTTP.Types (status404, status409)
import Utils

unit_tag_entry :: IO ()
unit_tag_entry = cofferTest do
  createEntry "tagged"

  response <- addOrRemoveTag POST "tagged" "tag"
  response @=
    [aesonQQ|
      {
        "path": "/tagged",
        "dateModified": "",
        "masterField": null,
        "fields": {},
        "tags": [
          "tag"
        ]
      }
    |]

unit_delete_tag :: IO ()
unit_delete_tag = cofferTest do
  unit_tag_entry

  response <- addOrRemoveTag DELETE "tagged" "tag"
  response @=
    [aesonQQ|
      {
        "path": "/tagged",
        "dateModified": "",
        "masterField": null,
        "fields": {},
        "tags": []
      }
    |]

unit_tag_entry_not_found :: IO ()
unit_tag_entry_not_found = cofferTest do
  try @HttpException (addOrRemoveTag POST "notexists" "tag") >>=
    processStatusCodeError status404
      [aesonQQ|
        [
          {
            "error": "Entry not found at '/notexists'.",
            "code": 700
          }
        ]
      |]

unit_tag_tag_not_found :: IO ()
unit_tag_tag_not_found = cofferTest do
  createEntry "entry"

  try @HttpException (addOrRemoveTag DELETE "entry" "notexists") >>=
    processStatusCodeError status404
      [aesonQQ|
        [
          {
            "error": "Entry does not have the tag 'notexists'.",
            "code": 701
          }
        ]
      |]

unit_tag_duplicate_tag :: IO ()
unit_tag_duplicate_tag = cofferTest do
  createEntry "entry"
  void $ addOrRemoveTag POST "entry" "tag"

  try @HttpException (addOrRemoveTag POST "entry" "tag") >>=
    processStatusCodeError status409
      [aesonQQ|
        [
          {
            "error": "Entry already has the tag 'tag'.",
            "code": 702
          }
        ]
      |]

unit_tag_updates_only_entry's_modification_time :: IO ()
unit_tag_updates_only_entry's_modification_time = cofferTest do
  createEntry "entry"
  response <- setField "entry" "field" "contents"
  let modificationTime = responseBody response ^?! key "dateModified"
  t1 <- getCurrentTime
  response <- addOrRemoveTag POST "entry" "tag"
  t2 <- getCurrentTime
  let newTime = responseBody response ^?! key "dateModified" . _JSON
  checkOnlyEntryModifiedDateUpdated (responseBody response) modificationTime t1 newTime t2 "field"
