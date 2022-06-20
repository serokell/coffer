-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backend
  ( BackendEffect (..)
  , readEntry
  , writeEntry
  , listDirectoryContents
  , deleteEntry
  , Backend (..)
  , SomeBackend (..)
  , Effects
  )
where

import BackendName (BackendName)
import Coffer.Path (DirectoryContents, EntryPath, Path)
import Entry (Entry)
import Error (CofferError)
import Polysemy
import Polysemy.Error (Error)
import Toml qualified
import Data.Aeson qualified as A

type Effects r = (Member (Embed IO) r, Member (Error CofferError) r)

class (Show a, A.FromJSON a) => Backend a where
  _name :: a -> BackendName
  _codec :: Toml.TomlCodec a
  _writeEntry :: Effects r => a -> Entry -> Sem r ()
  _readEntry :: Effects r => a -> EntryPath -> Sem r (Maybe Entry)
  _listDirectoryContents :: Effects r => a -> Path -> Sem r (Maybe DirectoryContents)
  _deleteEntry :: Effects r => a -> EntryPath -> Sem r ()

data SomeBackend where
  SomeBackend :: Backend a => a -> SomeBackend

instance Show SomeBackend where
  show (SomeBackend a) = show a

data BackendEffect m a where
  -- | Overwrites any entry that might already exist at that path.
  --   It does /not overwrite/ directories.
  --   If a directory with that path already exists, you'll end up with an entry /and/ a directory sharing the same path.
  WriteEntry :: SomeBackend -> Entry -> BackendEffect m ()
  ReadEntry :: SomeBackend -> EntryPath -> BackendEffect m (Maybe Entry)
  -- | Returns two lists of path segments:
  -- 1. list of path segments that represent directories
  -- 2. list of path segments that represent entries
  ListDirectoryContents :: SomeBackend -> Path -> BackendEffect m (Maybe DirectoryContents)
  -- | Once all entries are deleted from a directory, then the directory disappears
  --   (i.e. @ListDirectoryContents@ will no longer list that directory)
  DeleteEntry :: SomeBackend -> EntryPath -> BackendEffect m ()
makeSem ''BackendEffect
