-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backend.Interpreter where

import Backend (Backend(..), BackendEffect(..), Effects, SomeBackend(..))
import Polysemy

runBackend :: Effects r => Sem (BackendEffect ': r) a -> Sem r a
runBackend = interpret \case
  WriteSecret (SomeBackend backend) entry -> _writeSecret backend entry
  ReadSecret (SomeBackend backend) path -> _readSecret backend path
  ListSecrets (SomeBackend backend) path -> _listSecrets backend path
  DeleteSecret (SomeBackend backend) path -> _deleteSecret backend path
