-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Utils
  ( scrubDates
  , cofferTest
  , runVault
  , createEntry
  , deleteRecords
  , setField
  , changeFieldVisibility
  , findField
  , viewRoot
  , executeCommand
  , (@=)
  , processStatusCodeError
  , checkOnlyEntryModifiedDateUpdated
  , executeDeleteCommand
  ) where

import Control.Exception (Exception(displayException), try)
import Control.Lens
import Control.Monad (void)
import Data.Aeson (Object, Value(String), eitherDecodeStrict, encode)
import Data.Aeson.Lens
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Lazy qualified as BS (toStrict)
import Data.Generics
import Data.Text (Text)
import Data.Time
import GHC.IO.Handle (Handle)
import Network.HTTP.Client
  (HttpException(HttpExceptionRequest), HttpExceptionContent(StatusCodeException),
  Response(responseStatus))
import Network.HTTP.Req
import Network.HTTP.Req qualified as Req (HttpException)
import Network.HTTP.Types (Status)
import System.Process
  (CreateProcess(std_err, std_out), ProcessHandle, StdStream(NoStream), createProcess, proc)
import Test.Tasty.HUnit

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
deleteRecords = void $ try @Req.HttpException do
  executeCommand
    DELETE ["delete"]
    NoReqBody
    ignoreResponse
    ( mconcat
        [ "path" =: ("/" :: Text)
        , queryFlag "recursive"
        ]
    )

createEntry :: Text -> IO ()
createEntry path = void $
  executeCommand
    POST
    ["create"]
    (ReqBodyJson emptyNewEntry)
    ignoreResponse
    ("path" =: path)
  where
    emptyNewEntry =
      [aesonQQ|
        {
          "fields": {},
          "tags": []
        }
      |]

scrubDates :: Value -> Value
scrubDates =
  everywhere $ mkT @_ @Object \val ->
    val
      & ix "dateModified" .~ ""

-- | This function setups @vault@ for test.
cofferTest :: IO () -> IO ()
cofferTest test = deleteRecords >> test

setField :: Text -> Text -> Text -> IO (JsonResponse Value)
setField path name contents =
  executeCommand
    POST
    ["set-field"]
    (ReqBodyJson $ String contents)
    (jsonResponse @Value)
    ( mconcat
        [ "path" =: path
        , "field" =: name
        ]
    )

changeFieldVisibility :: Text -> Text -> Bool -> IO ()
changeFieldVisibility path field public = void $
  executeCommand
    POST
    ["set-field", visibility]
    NoReqBody
    ignoreResponse
    ( mconcat
        [ "path" =: path
        , "field" =: field
        ]
    )
  where
    visibility
      | public = "public"
      | otherwise = "private"

-- | Finds field in entry
findField :: Text -> Value -> Maybe Value
findField fieldName value =
  value ^? key "fields" . key fieldName

viewRoot :: IO Value
viewRoot = do
  response <-
    executeCommand
      GET
      ["view"]
      NoReqBody
      (jsonResponse @Value)
      ("path" =: ("/" :: Text))

  pure $ responseBody response

makeCofferHeader :: BS.ByteString
makeCofferHeader =  BS.toStrict . encode $
  [aesonQQ|
      {
        "type" : "vault-kv",
        "name" : "vault-local",
        "address" : "localhost:8213",
        "mount" : "secret",
        "token" : "root"
      }
  |]

executeCommand
  ::
   ( HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
   , HttpMethod method, HttpBody body, HttpResponse response
   )
   => method
   -> [Text]
   -> body
   -> Proxy response
   -> Option 'Http
   -> IO response
executeCommand method urlParts body proxy options =
  runReq defaultHttpConfig do
    req
      method
      url
      body
      proxy
      ( mconcat
          [ header "Coffer-Backend" makeCofferHeader
          , port 8081
          , options
          ]
      )
  where
    url = foldl (/:) baseUrl urlParts

(@=) :: JsonResponse Value -> Value -> IO ()
response @= value = scrubDates (responseBody response) @?= value

processStatusCodeError :: (Show res) => Status -> Value -> Either Req.HttpException res -> IO ()
processStatusCodeError expectedStatus jsonBodyExpected = \case
  Right res -> assertFailure $ "Expected http exception, got " <> show res
  Left (VanillaHttpException (HttpExceptionRequest _ (StatusCodeException response bs))) -> do
    jsonBodyActual <- either assertFailure pure $ eitherDecodeStrict @Value bs
    jsonBodyActual @?= jsonBodyExpected
    responseStatus response @?= expectedStatus
  Left httpExc -> assertFailure $ "Expected http exception, got " <> displayException httpExc

-- Checks, that after some modifying action only entry's modified date updates.
checkOnlyEntryModifiedDateUpdated :: Value -> Value -> UTCTime -> UTCTime -> UTCTime -> Text -> IO ()
checkOnlyEntryModifiedDateUpdated modifiedEntry modificationTime t1 newTime t2 fieldName = do
  assertBool "Date is not recent"
    $ t1 <= newTime && newTime <= t2

  assertBool "Dates are equal"
    $ modifiedEntry ^?! key "dateModified" /= modificationTime

  assertBool "Date are not equal"
    $ modifiedEntry ^?! key "fields" . key fieldName . key "dateModified" == modificationTime

executeDeleteCommand :: Text -> Bool -> IO IgnoreResponse
executeDeleteCommand path recursive =
  executeCommand
    DELETE
    ["delete"]
    NoReqBody
    ignoreResponse
    ( mconcat
        [ "path" =: path
        , if recursive then queryFlag "recursive" else mempty
        ]
    )
