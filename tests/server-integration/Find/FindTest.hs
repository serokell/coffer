-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Find.FindTest where

import Data.Aeson
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Network.HTTP.Req
import System.Time.Extra (sleep)
import Utils

unit_find_by_path :: IO ()
unit_find_by_path = cofferTest do
  createEntry "a/b"
  createEntry "a/c"
  createEntry "d/e"
  createEntry "d/f"

  response <-
    executeCommand
      GET
      ["find"]
      NoReqBody
      (jsonResponse @Value)
      ("path" =: ("a" :: Text))

  response @=
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
              }
            ],
            "subdirs": {}
          }
        }
      }
    |]

unit_find_by_text :: IO ()
unit_find_by_text = cofferTest do
  createEntry "a/text"
  createEntry "a/kek"
  createEntry "b/text"
  createEntry "b/kek"

  response <-
    executeCommand
      GET
      ["find"]
      NoReqBody
      (jsonResponse @Value)
      ("text" =: ("text" :: Text))

  response @=
    [aesonQQ|
      {
        "entries": [],
        "subdirs": {
          "a": {
            "entries": [
              {
                "path": "/a/text",
                "dateModified": "",
                "masterField": null,
                "fields": {},
                "tags": []
              }
            ],
            "subdirs": {}
          },
          "b": {
            "entries": [
              {
                "path": "/b/text",
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

unit_sort_field :: IO ()
unit_sort_field = cofferTest do
  createEntry "a"
  createEntry "c"
  createEntry "b"

  response <- applySortField "name:asc"
  response @=
    [aesonQQ|
      {
        "entries": [
          {
            "path": "/a",
            "dateModified": "",
            "masterField": null,
            "fields": {},
            "tags": []
          },
          {
            "path": "/b",
            "dateModified": "",
            "masterField": null,
            "fields": {},
            "tags": []
          },
          {
            "path": "/c",
            "dateModified": "",
            "masterField": null,
            "fields": {},
            "tags": []
          }
        ],
        "subdirs": {}
      }
    |]


  response <- applySortField "date:asc"
  response @=
    [aesonQQ|
      {
        "entries": [
          {
            "path": "/a",
            "dateModified": "",
            "masterField": null,
            "fields": {},
            "tags": []
          },
          {
            "path": "/c",
            "dateModified": "",
            "masterField": null,
            "fields": {},
            "tags": []
          },
          {
            "path": "/b",
            "dateModified": "",
            "masterField": null,
            "fields": {},
            "tags": []
          }
        ],
        "subdirs": {}
      }
    |]
  where
    applySortField :: Text -> IO (JsonResponse Value)
    applySortField sortField =
      executeCommand
        GET
        ["find"]
        NoReqBody
        (jsonResponse @Value)
        ("sort-field" =: sortField)

unit_filter :: IO ()
unit_filter = cofferTest do
  createEntry "a/kek1"
  createEntry "a/other1"
  sleep 1
  time <- getCurrentTime
  createEntry "b/kek2"
  createEntry "b/other2"

  let dateStr =
        formatTime defaultTimeLocale "%F %T" time
          & T.pack

  response <- applyFilter "name~kek"
  response @=
    [aesonQQ|
      {
        "entries": [],
        "subdirs": {
          "a": {
          "entries": [
            {
              "path": "/a/kek1",
              "dateModified": "",
              "masterField": null,
              "fields": {},
              "tags": []
            }
          ],
          "subdirs": {}
          },
          "b": {
            "entries": [
              {
                "path": "/b/kek2",
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

  response <- applyFilter $ "date>=" <> dateStr
  response @=
    [aesonQQ|
      {
        "entries": [],
        "subdirs": {
          "b": {
            "entries": [
              {
                "path": "/b/kek2",
                "dateModified": "",
                "masterField": null,
                "fields": {},
                "tags": []
              },
              {
                "path": "/b/other2",
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
  where
    applyFilter :: Text -> IO (JsonResponse Value)
    applyFilter filter =
      executeCommand
        GET
        ["find"]
        NoReqBody
        (jsonResponse @Value)
        ("filter" =: filter)
