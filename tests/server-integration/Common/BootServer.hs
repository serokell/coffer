-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Common.BootServer
  ( unit_run_with_cmd_option_port
  , unit_run_with_env_var_port
  , unit_run_with_no_port_specified
  , unit_run_with_cmd_opt_and_bad_env_var_port
  , unit_run_with_bad_cmd_opt_and_env_var_port
  , unit_run_with_bad_env_var_port
  , unit_run_with_bad_cmd_option_port
  , unit_run_with_bad_str_cmd_option_port
  ) where

import Control.Concurrent.Async (async, cancel, poll)
import Control.Exception (SomeException(SomeException))
import Control.Monad.Extra (whenJust)
import System.Environment (setEnv, unsetEnv, withArgs)
import System.IO (stderr)
import System.IO.Silently (hCapture)
import System.Time.Extra (sleep)
import Test.Tasty.HUnit (assertFailure, (@=?))
import Web.Main (RunServerException(..), runServer)

testPort :: String
testPort = "8079"

testServer :: Maybe String -> [String] -> IO () -> IO () -> (String -> IO ()) -> IO ()
testServer mbEnvPort args serverRunning serverStopped serverCrashed = do
  whenJust mbEnvPort $ setEnv "COFFER_SERVER_PORT"
  server <- async $ hCapture [stderr] $ withArgs args runServer
  sleep 1
  serverStatus <- poll server
  case serverStatus of
    Nothing -> cancel server >> serverRunning
    Just (Right _) -> serverStopped
    Just (Left (SomeException err)) -> serverCrashed $ show err
  whenJust mbEnvPort $ \_ -> unsetEnv "COFFER_SERVER_PORT"

unit_run_with_cmd_option_port :: IO ()
unit_run_with_cmd_option_port = testServer
  Nothing
  ["--port=" ++ testPort]
  (pure ())
  (assertFailure "Server ended its work")
  (\err -> assertFailure ("Server raised exception : " ++ show err))

-- can we hide stdout?
unit_run_with_bad_str_cmd_option_port :: IO()
unit_run_with_bad_str_cmd_option_port = testServer
  Nothing
  ["--port=abs"]
  (assertFailure "Server is running with bad port env var")
  (assertFailure "Server successfully ended its work with bad port env var")
  (\err -> "ExitFailure 1" @=? err)

unit_run_with_bad_cmd_option_port :: IO()
unit_run_with_bad_cmd_option_port = testServer
  Nothing
  ["--port=-1"]
  (assertFailure "Server is running with bad port env var")
  (assertFailure "Server successfully ended its work with bad port env var")
  (\err -> show (RunServerIncorrectPort -1) @=? err)

unit_run_with_env_var_port :: IO()
unit_run_with_env_var_port = testServer
  (Just testPort)
  []
  (pure ())
  (assertFailure "Server ended its work")
  (\err -> assertFailure $ "Server raised exception : " ++ show err)

unit_run_with_bad_env_var_port :: IO()
unit_run_with_bad_env_var_port = testServer
  (Just "bad_port")
  []
  (assertFailure "Server is running with bad port env var")
  (assertFailure "Server successfully ended its work with bad port env var")
  (\err -> show (RunServerEnvVarParseFail "Prelude.read: no parse") @=? err)

unit_run_with_no_port_specified :: IO()
unit_run_with_no_port_specified =  testServer
  Nothing
  []
  (assertFailure "Server is running without port specified")
  (assertFailure "Server successfully ended its work without port specified")
  (\err -> show RunServerNoPortSpecified @=? err)

unit_run_with_cmd_opt_and_bad_env_var_port :: IO()
unit_run_with_cmd_opt_and_bad_env_var_port = testServer
  (Just "-1")
  ["--port=" ++ testPort]
  (pure ())
  (assertFailure "Server ended its work")
  (\err -> assertFailure $ "Server raised exception : " ++ show err)

unit_run_with_bad_cmd_opt_and_env_var_port :: IO()
unit_run_with_bad_cmd_opt_and_env_var_port = testServer
  (Just testPort)
  ["--port=-1"]
  (assertFailure "Expected command-line option to take precedence over the env var, but the env var was used instead.")
  (assertFailure "Server ended its work")
  (\err -> show (RunServerIncorrectPort -1) @=? err)
