-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backend
  ( Backend (..)
  , Effects
  )
where

import BackendName (BackendName)
import Coffer.Path (DirectoryContents, EntryPath, Path)
import Data.Aeson qualified as A
import Entry (Entry)
import Error (CofferError)
import Polysemy
import Polysemy.Error (Error)
import Toml qualified

type Effects r = (Member (Embed IO) r, Member (Error CofferError) r)

class (Show a, A.FromJSON a) => Backend a where
  _name :: a -> BackendName
  _codec :: Toml.TomlCodec a
  _writeEntry :: Effects r => a -> Entry -> Sem r ()
  _readEntry :: Effects r => a -> EntryPath -> Sem r (Maybe Entry)
  _listDirectoryContents :: Effects r => a -> Path -> Sem r (Maybe DirectoryContents)
  _deleteEntry :: Effects r => a -> EntryPath -> Sem r ()
