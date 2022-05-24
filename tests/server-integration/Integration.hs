-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Integration where

import Control.Monad (void)
import Data.Aeson (Value(Object), (.:))
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Aeson.Types (parseEither)
import Data.Functor ((<&>))
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

  case responseBody response of
    Object obj -> do
      case parseEither (\o -> o .: "tag") obj of
        Left err -> assertFailure err
        Right (res :: Text) -> res @?= "CRSuccess"
    notObj -> assertFailure $ "Expected object, got: " <> show notObj

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

  actual <- getValueContents (responseBody response) <&> scrubDates
  actual @?= expected
  where
    expected =
      [aesonQQ|
        {
          "ePath": {
              "unEntryPath": [
                  "entry"
              ]
          },
          "eFields": {
              "public-field": {
                  "fVisibility": "public",
                  "fDateModified": "",
                  "fContents": "field1"
              },
              "private-field": {
                  "fVisibility": "private",
                  "fDateModified": "",
                  "fContents": "multiline\nfield"
              }
          },
          "eDateModified": "",
          "eMasterField": null,
          "eTags": [
              "first-tag",
              "second-tag"
          ]
        }
      |]
