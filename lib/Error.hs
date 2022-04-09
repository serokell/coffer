-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Error
  ( CofferError (..)
  ) where

import BackendName (BackendName)
import Data.Text qualified as T

data CofferError =
  MarshallingFailed
  | ConnectError
  | BackendNotFound BackendName
  | OtherError T.Text
  deriving stock (Show)
