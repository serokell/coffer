-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CopyAndRename.RenameTest where

import CopyAndRename.Common
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Functor ((<&>))
import Test.Tasty.HUnit
import Utils

unit_rename :: IO ()
unit_rename = cofferTest do
  testCopyOrRename False

  value <- viewRoot <&> scrubDates
  value @?= rootState
  where
    rootState =
      [aesonQQ|
        {
          "entries": [],
          "subdirs": {
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

unit_rename_path_not_found :: IO ()
unit_rename_path_not_found = copyOrRenamePathNotFound False

unit_rename_missing_entry_name :: IO ()
unit_rename_missing_entry_name = copyOrRenameMissingEntryName False

unit_rename_same_path :: IO ()
unit_rename_same_path = copyOrRenameSamePath False

unit_rename_create_errors :: IO ()
unit_rename_create_errors = copyOrRenameCreateErrors False

unit_rename_updates_only_entry's_modification_time :: IO ()
unit_rename_updates_only_entry's_modification_time =
  copyOrRenameUpdatesOnlyEntrysModificationTime False
