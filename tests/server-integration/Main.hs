-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Concurrent.Async (async, cancel)
import Data.Functor ((<&>))
import System.Environment (withArgs)
import System.Process (terminateProcess)
import System.Time.Extra (sleep)
import Test.Tasty (defaultMain, localOption, withResource)
import Test.Tasty.Runners (NumThreads(NumThreads))
import Tree (tests)
import Utils
import Web.Main (runServer)

main :: IO ()
main = do
  tests' <- tests <&> localOption (NumThreads 1)

  let testTree =
        withResource
          do
            server <- async $ withArgs ["--port=8081"] runServer
            (_, _, _, vault1) <- runVault 8213 "root"
            (_, _, _, vault2) <- runVault 8215 "second"
            sleep 1
            pure (server, vault1, vault2)
          do
            \(server, vault1, vault2) -> do
              cancel server
              terminateProcess vault1
              terminateProcess vault2
          do \_ -> tests'

  defaultMain testTree
