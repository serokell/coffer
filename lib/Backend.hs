-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Backend
  ( BackendEffect (..), readSecret, writeSecret, listSecrets, deleteSecret
  , Backend (..)
  , SomeBackend (..)
  )
where

import qualified Data.Text           as T
import qualified Entry               as E
import qualified Toml

import Error                         (CofferError)
import Polysemy.Error                (Error)

import Polysemy
import Coffer.Path (EntryPath, Path)

-- @TODO - rename Secret to Entry?
data BackendEffect m a where
  -- | Overwrites any entry that might already exist at that path.
  --   It does /not overwrite/ directories.
  --   If a directory with that path already exists, you'll end up with an entry /and/ a directory sharing the same path.
  WriteSecret  :: E.Entry -> BackendEffect m ()
  -- | Returns path segments: if the segment is suffixed by @/@ then that indicates a directory;
  --   otherwise it's an entry
  ReadSecret   :: EntryPath -> BackendEffect m (Maybe E.Entry)
  ListSecrets  :: Path -> BackendEffect m (Maybe [T.Text])
  -- | Once all entries are deleted from a directory, then the directory disappears
  --   (i.e. @ListSecrets@ will no longer list that directory)
  DeleteSecret :: EntryPath -> BackendEffect m ()
makeSem ''BackendEffect

class Show a => Backend a where
  _name :: a -> T.Text
  _codec :: Toml.TomlCodec a
  _runEffect :: Member (Embed IO) r
             => Member (Error CofferError) r
             => a
             -> Sem (BackendEffect ': r) t
             -> Sem r t

data SomeBackend where
  SomeBackend :: Backend a => a -> SomeBackend

instance Show SomeBackend where
  show (SomeBackend a) = show a
