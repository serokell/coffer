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

  responseBody response @?= [aesonQQ| { "path": "/entry" } |]

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
          "tags": [
              "first-tag",
              "second-tag"
          ],
          "masterField": null,
          "path": "/entry",
          "fields": {
              "public-field": {
                  "contents": "field1",
                  "visibility": "public",
                  "dateModified": ""
              },
              "private-field": {
                  "contents": "multiline\nfield",
                  "visibility": "private",
                  "dateModified": ""
              }
          },
          "dateModified": ""
        }
      |]
