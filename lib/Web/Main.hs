-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Web.Main
  ( runServer
  , RunServerException (..)
  ) where

import Backend.Commands (runCommand)
import Config
import Control.Exception (Exception, throw)
import Data.Bifunctor (Bifunctor(first))
import Data.Either (partitionEithers)
import Data.Proxy
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import System.Environment (lookupEnv)
import Text.Read (readEither)
import Web.API (API)
import Web.Server
import Options.Applicative
import Text.Interpolation.Nyan

data ServerOptions = ServerOptions { serverPort :: Maybe Int }

data RunServerException
  = RunServerIncorrectPort Int
  | RunServerNoPortSpecified
  | RunServerEnvVarParseFail String
  deriving stock (Eq)

instance Show RunServerException where
  show (RunServerIncorrectPort port) = "Port number should be greater then 0. Your port number is: " ++ show port
  show RunServerNoPortSpecified = "No port where not specified. Can be set by envirment varieble \"COFFER_SERVER_PORT\" or by \"--port=$port_num\" cmd argument."
  show (RunServerEnvVarParseFail msg) = "Can't parse port specified by env var \"COFFER_SERVER_PORT\": " ++ msg

instance Exception RunServerException


parseServerOptions :: ParserInfo ServerOptions
parseServerOptions = info (parser <**> helper) mempty

parser ::  Parser ServerOptions
parser = ServerOptions <$> parseServerOptionPort

parseServerOptionPort :: Parser (Maybe Int)
parseServerOptionPort = optional $ option auto $ mconcat
        [ long "port"
        , short 'p'
        , metavar "COFFER_SERVER_PORT"
        , help [int|s|
            Specify server port to run a server.
            When this option is not set, the 'COFFER_SERVER_PORT' environment variable will be used.
            When neither is set, it will crash.
          |]
        ]


runServer :: IO ()
runServer =
  do
  opts <- execParser parseServerOptions
  envPortStr <- lookupEnv "COFFER_SERVER_PORT"

  let envPort = maybe (Left RunServerNoPortSpecified) ((first RunServerEnvVarParseFail) . readEither) envPortStr
  let optPort = maybe (Left RunServerNoPortSpecified) Right $ serverPort opts

  let (errs, ports) = partitionEithers [optPort, envPort]

  case ports of
    (port : _) -> do
      if 0 < port
        then run port $ serve (Proxy :: Proxy API) do
          makeServer \someBackend ->
            reportErrors . runBackendIO' . runCommand (makeSingleBackendConfig  someBackend)
        else throw $ RunServerIncorrectPort port
    _ -> throw $ last errs
