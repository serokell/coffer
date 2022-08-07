-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backend.Interpreter where

import Backend (Backend(..), Effects)
import BackendEffect (BackendEffect(..))
import Backends (SomeBackend(..))
import Polysemy

runBackend :: Effects r => Sem (BackendEffect ': r) a -> Sem r a
runBackend = interpret \case
  WriteEntry (SomeBackend backend) entry -> _writeEntry backend entry
  ReadEntry (SomeBackend backend) path -> _readEntry backend path
  ListDirectoryContents (SomeBackend backend) path -> _listDirectoryContents backend path
  DeleteEntry (SomeBackend backend) path -> _deleteEntry backend path
  ValidatePath (SomeBackend backend) path -> _validatePath backend path
