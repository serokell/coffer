-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Utils
  ( scrubDates
  , cofferTest
  , runVault
  , baseUrl
  , exampleEntry
  ) where

import Control.Exception (try)
import Control.Lens
import Control.Monad (void)
import Data.Aeson (Object, Value)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Generics
import Data.Text (Text)
import GHC.IO.Handle (Handle)
import Network.HTTP.Req
import System.Process
  (CreateProcess(std_err, std_out), ProcessHandle, StdStream(NoStream), createProcess, proc)

procWithoutOutput :: CreateProcess -> CreateProcess
procWithoutOutput cp = cp
  { std_out = NoStream
  , std_err = NoStream
  }

runVault :: Int -> String -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
runVault port tokenId = createProcess process
  where
    process = procWithoutOutput $
      proc "vault"
        [ "server"
        , "-dev"
        , "-dev-root-token-id=" <> tokenId
        , "-dev-listen-address=localhost:" <> show port
        ]

baseUrl :: Url 'Http
baseUrl = http "localhost" /: "api" /: "v1" /: "content"

deleteRecords :: IO ()
deleteRecords = void $ try @HttpException do
  runReq defaultHttpConfig do
    req
      DELETE
      (baseUrl /: "delete")
      NoReqBody
      ignoreResponse
      ( mconcat
          [ header "token" "root"
          , "path" =: ("/" :: Text)
          , queryFlag "recursive"
          , port 8081
          ]
      )

scrubDates :: Value -> Value
scrubDates =
  everywhere $ mkT @_ @Object \val ->
    val
      & ix "dateModified" .~ ""

-- | This function setups @vault@ for test.
cofferTest :: IO () -> IO ()
cofferTest test = deleteRecords >> test

exampleEntry :: Value
exampleEntry =
  [aesonQQ|
    {
      "fields":
        {
          "public-field": { "contents": "field1", "visibility": "public" },
          "private-field": { "contents": "multiline\nfield", "visibility": "private"}
        },
      "tags": ["first-tag", "second-tag"]
    }
  |]
