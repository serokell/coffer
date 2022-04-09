-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backend
  ( BackendEffect (..)
  , readSecret
  , writeSecret
  , listSecrets
  , deleteSecret
  , Backend (..)
  , SomeBackend (..)
  , Effects
  )
where

import BackendName (BackendName)
import Coffer.Path (EntryPath, Path)
import Data.Text (Text)
import Entry (Entry)
import Error (CofferError)
import Polysemy
import Polysemy.Error (Error)
import Toml qualified

type Effects r = (Member (Embed IO) r, Member (Error CofferError) r)

class Show a => Backend a where
  _name :: a -> BackendName
  _codec :: Toml.TomlCodec a
  _writeSecret :: Effects r => a -> Entry -> Sem r ()
  _readSecret :: Effects r => a -> EntryPath -> Sem r (Maybe Entry)
  _listSecrets :: Effects r => a -> Path -> Sem r (Maybe [Text])
  _deleteSecret :: Effects r => a -> EntryPath -> Sem r ()

data SomeBackend where
  SomeBackend :: Backend a => a -> SomeBackend

instance Show SomeBackend where
  show (SomeBackend a) = show a

-- @TODO - rename Secret to Entry?
data BackendEffect m a where
  -- | Overwrites any entry that might already exist at that path.
  --   It does /not overwrite/ directories.
  --   If a directory with that path already exists, you'll end up with an entry /and/ a directory sharing the same path.
  WriteSecret  :: SomeBackend -> Entry -> BackendEffect m ()
  -- | Returns path segments: if the segment is suffixed by @/@ then that indicates a directory;
  --   otherwise it's an entry
  ReadSecret   :: SomeBackend -> EntryPath -> BackendEffect m (Maybe Entry)
  ListSecrets  :: SomeBackend -> Path -> BackendEffect m (Maybe [Text])
  -- | Once all entries are deleted from a directory, then the directory disappears
  --   (i.e. @ListSecrets@ will no longer list that directory)
  DeleteSecret :: SomeBackend -> EntryPath -> BackendEffect m ()
makeSem ''BackendEffect
