-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module CLI.EntryView
  ( FieldInfoView (..)
  , fieldInfo
  , private
  , EntryView (..)
  , qEntryPath
  , entryTags
  , fields
  , entryViewCodec
  ) where

import CLI.Types (FieldInfo(FieldInfo, fiContents, fiName))
import Coffer.Path (EntryPath, QualifiedPath, mkQualifiedEntryPath)
import Coffer.Util (didimatch)
import Control.Lens
import Data.Bifunctor (Bifunctor(first))
import Data.Set (Set)
import Entry
  (EntryTag, FieldKey, FieldValue(FieldValue, unFieldValue), getEntryTag, getFieldKey, newEntryTag,
  newFieldKey)
import Fmt (Buildable(build), fmt)
import Toml qualified

data FieldInfoView = FieldInfoView
  { fivFieldInfo :: FieldInfo
  , fivPrivate   :: Bool
  }
  deriving stock (Show)
makeLensesWith abbreviatedFields ''FieldInfoView

data EntryView = EntryView
  { evQEntryPath :: QualifiedPath EntryPath
  , evEntryTags  :: Set EntryTag
  , evFields     :: [FieldInfoView]
  }
  deriving stock (Show)
makeLensesWith abbreviatedFields ''EntryView

fieldInfoViewCodec :: Toml.TomlCodec FieldInfoView
fieldInfoViewCodec = FieldInfoView
  <$> fieldInfoCodec      Toml..= fivFieldInfo
  <*> Toml.bool "private" Toml..= fivPrivate
  where
    fieldKeyCodec :: Toml.TomlCodec FieldKey
    fieldKeyCodec = didimatch (Right . getFieldKey) newFieldKey (Toml.text "name")

    fieldValueCodec :: Toml.TomlCodec FieldValue
    fieldValueCodec = Toml.dimap unFieldValue FieldValue (Toml.text "contents")

    fieldInfoCodec :: Toml.TomlCodec FieldInfo
    fieldInfoCodec = FieldInfo
      <$> fieldKeyCodec   Toml..= fiName
      <*> fieldValueCodec Toml..= fiContents

entryPathCodec :: Toml.TomlCodec (QualifiedPath EntryPath)
entryPathCodec = didimatch (Right . fmt . build) mkQualifiedEntryPath (Toml.text "path")

_entryTag :: Toml.TomlBiMap EntryTag Toml.AnyValue
_entryTag = Toml.BiMap to from
  where
    to :: EntryTag -> Either Toml.TomlBiMapError Toml.AnyValue
    to = Toml.forward Toml._Text . getEntryTag

    from :: Toml.AnyValue -> Either Toml.TomlBiMapError EntryTag
    from value = do
      txt <- Toml.backward Toml._Text value
      newEntryTag txt & first Toml.ArbitraryError

entryTagsCodec :: Toml.TomlCodec (Set EntryTag)
entryTagsCodec = Toml.arraySetOf _entryTag "tags"

entryViewCodec :: Toml.TomlCodec EntryView
entryViewCodec = EntryView
  <$> entryPathCodec                       Toml..= evQEntryPath
  <*> entryTagsCodec                       Toml..= evEntryTags
  <*> Toml.list fieldInfoViewCodec "field" Toml..= evFields
