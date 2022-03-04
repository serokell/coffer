-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Main where

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = mainFromCabal "coffer" =<< getArgs
