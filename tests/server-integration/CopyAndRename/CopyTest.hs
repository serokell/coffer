-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CopyAndRename.CopyTest where

import Control.Lens
import CopyAndRename.Common
import Data.Aeson.QQ.Simple (aesonQQ)
import Test.Tasty.HUnit
import Utils

unit_copy :: IO ()
unit_copy = cofferTest do
  testCopyOrRename True

  value <- viewRoot <&> scrubDates
  value @?= rootState
  where
    rootState =
      [aesonQQ|
        {
          "entries": [],
          "subdirs": {
            "a": {
              "entries": [
                {
                  "path": "/a/b",
                  "dateModified": "",
                  "masterField": null,
                  "fields": {},
                  "tags": []
                },
                {
                  "path": "/a/c",
                  "dateModified": "",
                  "masterField": null,
                  "fields": {},
                  "tags": []
                },
                {
                  "path": "/a/d",
                  "dateModified": "",
                  "masterField": null,
                  "fields": {},
                  "tags": []
                }
              ],
              "subdirs": {}
            },
            "aCopy": {
              "entries": [
                {
                  "path": "/aCopy/b",
                  "dateModified": "",
                  "masterField": null,
                  "fields": {},
                  "tags": []
                },
                {
                  "path": "/aCopy/c",
                  "dateModified": "",
                  "masterField": null,
                  "fields": {},
                  "tags": []
                },
                {
                  "path": "/aCopy/d",
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

unit_copy_path_not_found :: IO ()
unit_copy_path_not_found = copyOrRenamePathNotFound True

unit_copy_missing_entry_name :: IO ()
unit_copy_missing_entry_name = copyOrRenameMissingEntryName True

unit_copy_same_path :: IO ()
unit_copy_same_path = copyOrRenameSamePath True

unit_copy_create_errors :: IO ()
unit_copy_create_errors = copyOrRenameCreateErrors True

unit_copy_updates_only_entry's_modification_time :: IO ()
unit_copy_updates_only_entry's_modification_time =
  copyOrRenameUpdatesOnlyEntrysModificationTime True
