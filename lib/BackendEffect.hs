-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module BackendEffect
  ( BackendEffect (..)
  , readEntry
  , writeEntry
  , listDirectoryContents
  , deleteEntry ) where

import Backends (SomeBackend)
import Coffer.Path (DirectoryContents, EntryPath, Path)
import Entry (Entry)
import Polysemy

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
