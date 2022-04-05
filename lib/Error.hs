-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Error
  ( CofferError (..)
  ) where

import BackendName (BackendName)
import Fmt (Buildable(build), Builder, indentF, unlinesF)
import Servant.Client.Core
  (ClientError(ConnectionError, DecodeFailure, FailureResponse, InvalidContentTypeHeader, UnsupportedContentType))

data CofferError
  = ServantError ClientError
  | BackendNotFound BackendName
  | OtherError Builder
  deriving stock (Show)

instance Buildable CofferError where
  build = \case
    ServantError (FailureResponse request response) ->
      unlinesF @_ @Builder
        [ "Request:"
        , indentF 2 ((build . show) request)
        , "failed with response:"
        , indentF 2 ((build . show) response)
        ]
    ServantError (DecodeFailure body response) ->
      unlinesF @_ @Builder
        [ "The body could not be decoded at the expected type."
        , "Body: " <> build body
        , "Response:"
        , indentF 2 ((build . show) response)
        ]
    ServantError (UnsupportedContentType mediatype response) ->
      unlinesF @_ @Builder
        [ "The content-type '" <> (build . show) mediatype <> "' of the response is not supported."
        , "Response:"
        , indentF 2 ((build . show) response)
        ]
    ServantError (InvalidContentTypeHeader response) ->
      unlinesF @_ @Builder
        [ "The content-type header is invalid."
        , "Response:"
        , indentF 2 ((build . show) response)
        ]
    ServantError (ConnectionError exception) ->
      unlinesF @_ @Builder
        [ "Connection error. No response was received."
        , (build . show) exception
        ]
    BackendNotFound backendName -> "Backend with name '" <> build backendName <> "' not found."
    OtherError t ->
      unlinesF @_ @Builder
        [ "An internal error occurred:"
        , t
        ]
