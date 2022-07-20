-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Common.BootServer
  ( unit_run_with_cmd_option_port
  , unit_run_with_env_var_port
  , unit_run_with_no_port_specified
  , unit_run_with_cmd_opt_and_bad_env_var_port
  , unit_run_with_bad_cmd_opt_and_env_var_port
  , unit_run_with_bad_env_var_port) where

import Control.Concurrent.Async (async, cancel, poll)
import Control.Exception (SomeException(SomeException))
import System.Environment (setEnv, unsetEnv, withArgs)
import System.Time.Extra (sleep)
import Test.Tasty.HUnit (assertFailure, (@=?))
import Web.Main (RunServerException(..), runServer)

testPort :: String
testPort = "8079"

testServer :: String -> [String] -> IO () -> IO () -> (String -> IO ()) -> IO ()
testServer envPort args serverRunning serverStopped serverCrashed = do
  setEnv "COFFER_SERVER_PORT" envPort
  server <- async $ withArgs args runServer
  sleep 1
  serverStatus <- poll server
  case serverStatus of
    Nothing -> cancel server >> serverRunning
    Just (Right _) -> serverStopped
    Just (Left (SomeException err)) -> serverCrashed $ show err
  unsetEnv "COFFER_SERVER_PORT"

unit_run_with_cmd_option_port :: IO ()
unit_run_with_cmd_option_port = testServer
  ""
  ["--port=" ++ testPort]
  (pure ())
  (assertFailure "Server ended it's work")
  (\err -> assertFailure ("Server raised exception : " ++ show err))

unit_run_with_env_var_port :: IO()
unit_run_with_env_var_port = testServer
  testPort
  []
  (pure ())
  (assertFailure "Server ended it's work")
  (\err -> assertFailure $ "Server raised exception : " ++ show err)

unit_run_with_bad_env_var_port :: IO()
unit_run_with_bad_env_var_port = testServer
  "bad_port"
  []
  (assertFailure "Server is runnung with bad port env var")
  (assertFailure "Server successfully ended it's work with bad port env var")
  (\err -> show (RunServerEnvVarParseFail "Prelude.read: no parse") @=? err)

unit_run_with_no_port_specified :: IO()
unit_run_with_no_port_specified =  testServer
  ""
  []
  (assertFailure "Server is runnung without port specified")
  (assertFailure "Server successfully ended it's work without port specified")
  (\err -> show RunServerNoPortSpecified @=? err)

unit_run_with_cmd_opt_and_bad_env_var_port :: IO()
unit_run_with_cmd_opt_and_bad_env_var_port = testServer
  "-1"
  ["--port=" ++ testPort]
  (pure ())
  (assertFailure "Server ended it's work")
  (\err -> assertFailure $ "Server raised exception : " ++ show err)


unit_run_with_bad_cmd_opt_and_env_var_port :: IO()
unit_run_with_bad_cmd_opt_and_env_var_port = testServer
  testPort
  ["--port=-1"]
  (assertFailure "Server made prefernce to env var instead of cmd option")
  (assertFailure "Server ended it's work")
  (\err -> show (RunServerIncorrectPort -1) @=? err)
