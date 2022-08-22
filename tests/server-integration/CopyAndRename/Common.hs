-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CopyAndRename.Common
  ( testCopyOrRename
  , copyOrRenamePathNotFound
  , copyOrRenameMissingEntryName
  , copyOrRenameSamePath
  , copyOrRenameCreateErrors
  , copyOrRenameUpdatesOnlyEntrysModificationTime
  ) where

import Control.Exception (try)
import Control.Lens
import Control.Monad (void, when)
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Text (Text)
import Data.Time
import Network.HTTP.Req
import Network.HTTP.Types (status400, status404, status409)
import Utils

endpoint :: Bool -> Text
endpoint copy
  | copy = "copy"
  | otherwise = "rename"

executeCopyOrRename :: Bool -> Text -> Text -> IO (JsonResponse Value)
executeCopyOrRename copy oldPath newPath =
  executeCommand
    POST
    [endpoint copy]
    NoReqBody
    (jsonResponse @Value)
    ( mconcat
        [ "old-path" =: oldPath
        , "new-path" =: newPath
        ]
    )

testCopyOrRename :: Bool -> IO ()
testCopyOrRename copy = cofferTest do
  createEntry "a/b"
  createEntry "a/c"
  createEntry "a/d"

  response <- executeCopyOrRename copy "a" "aCopy"
  response @=
    [aesonQQ|
      [
        {
          "from": "/a/b",
          "to": "/aCopy/b"
        },
        {
          "from": "/a/c",
          "to": "/aCopy/c"
        },
        {
          "from": "/a/d",
          "to": "/aCopy/d"
        }
      ]
    |]

copyOrRenamePathNotFound :: Bool -> IO ()
copyOrRenamePathNotFound copy = cofferTest do
  try @HttpException (executeCopyOrRename copy "notexist" "/") >>=
    processStatusCodeError status404
      [aesonQQ|
        [
          {
            "error": "Entry or directory not found at '/notexist'.\n",
            "code": 500
          }
        ]
      |]

copyOrRenameMissingEntryName :: Bool -> IO ()
copyOrRenameMissingEntryName copy = cofferTest do
  createEntry "entry"

  try @HttpException (executeCopyOrRename copy "entry" "/") >>=
    processStatusCodeError status400
      [aesonQQ|
        [
          {
            "error": "The destination path is not a valid entry path. Please specify the new name of the entry.\n",
            "code": 501
          }
        ]
      |]

copyOrRenameSamePath :: Bool -> IO ()
copyOrRenameSamePath copy = cofferTest do
  createEntry "same"

  try @HttpException (executeCopyOrRename copy "same" "same") >>=
    processStatusCodeError status409
      [aesonQQ|
        [
          {
            "error": "'/same' and '/same' are the same path.\n",
            "code": 502
          }
        ]
      |]

copyOrRenameCreateErrors :: Bool -> IO ()
copyOrRenameCreateErrors copy = cofferTest do
  createEntry "a/b"
  createEntry "a/d"
  createEntry "x/d"
  createEntry "x/b/c"

  try @HttpException (executeCopyOrRename copy "a" "x") >>=
    processStatusCodeError status409
      [aesonQQ|
        [
          {
            "error": "'/a/b' to '/x/b':\n  '/x/b' is a directory.\n",
            "code": 504
          },
          {
            "error": "'/a/d' to '/x/d':\n  An entry already exists at '/x/d'.\n  Use 'force' query flag to overwrite existing entries.\n",
            "code": 505
          }
        ]
      |]

  deleteRecords

  createEntry "a/b"
  createEntry "c/d/e"

  try @HttpException (executeCopyOrRename copy "c/d/e" "a/b/c") >>=
    processStatusCodeError status409
      [aesonQQ|
        [
          {
            "error": "'/c/d/e' to '/a/b/c':\n  Attempted to create the directory '/a/b' but an entry exists at that path.\n",
            "code": 503
          }
        ]
      |]

copyOrRenameUpdatesOnlyEntrysModificationTime :: Bool -> IO ()
copyOrRenameUpdatesOnlyEntrysModificationTime copy = cofferTest do
  createEntry "entry"
  response <- setField "entry" "field" "contents"
  let modifiedDate = responseBody response ^?! key "dateModified"

  t1 <- getCurrentTime
  executeCopyOrRename copy "entry" "newEntry"
  t2 <- getCurrentTime

  when copy do
    void $ executeDeleteCommand "entry" False

  dir <- viewRoot

  let modifiedEntry = dir ^?! key "entries" . nth 0
  let newTime = modifiedEntry ^?! key "dateModified" . _JSON

  checkOnlyEntryModifiedDateUpdated modifiedEntry modifiedDate t1 newTime t2 "field"
