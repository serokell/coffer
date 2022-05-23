-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Coffer.Directory
  ( Directory(..)
  , entries
  , subdirs
  , emptyDir
  , singleton
  , allEntries
  , mapMaybe
  , mapEntry
  , mapDir
  , insertEntry
  , trimEmptyDirs
  , filterEntries
  ) where

import Coffer.Path (PathSegment, entryPathParentDir, pathSegments)
import Control.Lens
import Data.Aeson (toJSON)
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe qualified as Maybe
import Data.OpenApi
import Data.OpenApi.Lens qualified as Schema
import Entry (Entry, exampleEntry)
import Entry qualified as E
import GHC.Generics (Generic)

----------------------------------------------------------------------------
-- Directory
----------------------------------------------------------------------------

data Directory = Directory
  { dEntries :: [Entry]
  , dSubdirs :: HashMap PathSegment Directory
  }
  deriving stock (Show, Eq, Generic)
deriveToJSON (aesonPrefix camelCase) ''Directory

makeLensesWith abbreviatedFields 'Directory

instance ToSchema Directory where
  declareNamedSchema proxy =
    genericDeclareNamedSchema (fromAesonOptions (aesonPrefix camelCase)) proxy
      & mapped . Schema.schema . example ?~ toJSON (insertEntry exampleEntry emptyDir)

emptyDir :: Directory
emptyDir = Directory mempty mempty

singleton :: Entry -> Directory
singleton entry = insertEntry entry emptyDir

allEntries :: Directory -> [Entry]
allEntries (Directory entries subdirs) =
  entries <> foldMap allEntries subdirs

mapMaybe :: (Entry -> Maybe Entry) -> Directory -> Directory
mapMaybe f (Directory entries subdirs) =
  Directory
    (Maybe.mapMaybe f entries)
    (mapMaybe f <$> subdirs)

-- | Map over each entry.
mapEntry :: (Entry -> Entry) -> Directory -> Directory
mapEntry f = mapDir (fmap f)

-- | Map over each directory's contents.
-- This can be used for sorting directories' entries.
mapDir :: ([Entry] -> [Entry]) -> Directory -> Directory
mapDir f (Directory entries subdirs) =
  Directory
    (f entries)
    (mapDir f <$> subdirs)

-- | Insert an entry at the given path.
insertEntry :: Entry -> Directory -> Directory
insertEntry newEntry = go (newEntry ^. E.path . entryPathParentDir . pathSegments)
  where
    go :: [PathSegment] -> Directory -> Directory
    go [] dir =
      -- Insert entry in the current directory
      dir & entries <>~ [newEntry]
    go (dirName : rest) dir =
      -- Does the subdirectory we're looking for already exist?
      dir & subdirs . at dirName %~ \case
        Nothing -> Just $ mkNewDir rest
        Just subdir -> Just $ go rest subdir

    mkNewDir :: [PathSegment] -> Directory
    mkNewDir [] = Directory [newEntry] mempty
    mkNewDir (dirName : rest) =
      Directory [] $
        HashMap.singleton dirName (mkNewDir rest)

-- | Delete directories that have no entries or subdirectories.
-- Returns `Nothing` if there are no entries at all in the entire tree.
trimEmptyDirs :: Directory -> Maybe Directory
trimEmptyDirs dir = do
  let dirTrimmed = dir & subdirs %~ HashMap.mapMaybe trimEmptyDirs
  if null (dirTrimmed ^. entries) && null (dirTrimmed ^. subdirs)
    then Nothing
    else Just dirTrimmed

filterEntries :: (Entry -> Bool) -> Directory -> Directory
filterEntries predicate =
  mapMaybe \entry ->
    if predicate entry then Just entry else Nothing
