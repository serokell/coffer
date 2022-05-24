-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Utils
  ( scrubDates
  , getValueContents
  , cofferTest
  , runVault
  , baseUrl
  , exampleEntry
  ) where

import Control.Lens
import Control.Monad (void)
import Data.Aeson (Object, Value)
import Data.Aeson.Lens
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.Generics
import Data.Text (Text)
import GHC.IO.Handle (Handle)
import Network.HTTP.Req
import System.Process
  (CreateProcess(std_err, std_out), ProcessHandle, StdStream(NoStream), createProcess, proc)
import Test.Tasty.HUnit (assertFailure)

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
deleteRecords = runReq defaultHttpConfig do
  void
    $ req
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
      & ix "eDateModified" .~ ""
      & ix "fDateModified" .~ ""

getValueContents :: Value -> IO Value
getValueContents value = do
  case value ^? key "contents" of
    Just res -> pure res
    Nothing -> assertFailure $ "Expected field \"contents\", but don't get it: " <> show value

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
