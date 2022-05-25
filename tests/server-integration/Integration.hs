-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Integration where

import Control.Monad (void)
import Data.Aeson (Value)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Text (Text)
import Network.HTTP.Req
import Test.Tasty.HUnit
import Utils

unit_create_an_entry :: IO ()
unit_create_an_entry = cofferTest do
  response <-
    runReq defaultHttpConfig do
      req
        POST
        (baseUrl /: "create")
        (ReqBodyJson exampleEntry)
        (jsonResponse @Value)
        ( mconcat
            [ header "token" "root"
            , "path" =: ("entry" :: Text)
            , port 8081
            ]
        )

  scrubDates (responseBody response) @?= expected
  where
    expected =
      [aesonQQ|
        {
          "path": "/entry",
          "dateModified": "",
          "masterField": null,
          "fields": {
              "public-field": {
                  "dateModified": "",
                  "visibility": "public",
                  "contents": "field1"
              },
              "private-field": {
                  "dateModified": "",
                  "visibility": "private",
                  "contents": "multiline\nfield"
              }
          },
          "tags": [
              "first-tag",
              "second-tag"
          ]
        }
      |]

unit_view_an_entry :: IO ()
unit_view_an_entry = cofferTest do
  response <-
    runReq defaultHttpConfig do
      void $ req
        POST
        (baseUrl /: "create")
        (ReqBodyJson exampleEntry)
        ignoreResponse
        ( mconcat
            [ header "token" "root"
            , "path" =: ("entry" :: Text)
            , port 8081
            ]
        )

      req
        GET
        (baseUrl /: "view")
        NoReqBody
        (jsonResponse @Value)
        ( mconcat
            [ header "token" "root"
            , "path" =: ("entry" :: Text)
            , port 8081
            ]
        )

  scrubDates (responseBody response) @?= expected
  where
    expected =
      [aesonQQ|
        {
          "entries": [
              {
                "path": "/entry",
                "dateModified": "",
                "masterField": null,
                "fields": {
                    "public-field": {
                        "dateModified": "",
                        "visibility": "public",
                        "contents": "field1"
                    },
                    "private-field": {
                        "dateModified": "",
                        "visibility": "private",
                        "contents": "multiline\nfield"
                    }
                },
                "tags": [
                    "first-tag",
                    "second-tag"
                ]
              }
          ],
          "subdirs": {}
        }
      |]
