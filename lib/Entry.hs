-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Entry
  ( dateModified
  , Entry, EntryConvertible (..), newEntry
  , path, masterField, fields
  , Field (..), FieldKey,  newField, getFieldKey
  , newFieldKey, newEntryTag, getEntryTag
  , visibility, value, tags, EntryTag
  , FieldVisibility(..)
  )
where


import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HS
import qualified Data.Set as S

import Control.Lens
import qualified Data.Aeson as A
import Data.Hashable (Hashable)
import Data.Time (UTCTime)
import Coffer.Path (EntryPath)

newtype FieldKey = UnsafeFieldKey T.Text
  deriving stock (Show, Eq)
  deriving newtype (A.ToJSON, A.ToJSONKey, A.FromJSON, A.FromJSONKey, Hashable)

keyCharSet :: [Char]
keyCharSet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_;"

newFieldKey :: T.Text -> Maybe FieldKey
newFieldKey t =
  if T.all (`elem` keyCharSet) t && not (T.null t) then
    Just $ UnsafeFieldKey t
  else
    Nothing

getFieldKey :: FieldKey -> T.Text
getFieldKey (UnsafeFieldKey t) = t

newtype EntryTag = UnsafeEntryTag T.Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (A.ToJSON, A.FromJSON)

newEntryTag :: T.Text -> Maybe EntryTag
newEntryTag t =
  if T.all (`elem` keyCharSet) t && not (T.null t) then
    Just $ UnsafeEntryTag t
  else
    Nothing

getEntryTag :: EntryTag -> T.Text
getEntryTag (UnsafeEntryTag t) = t

data FieldVisibility = Public | Private
  deriving stock (Show, Eq)

instance A.ToJSON FieldVisibility where
  toJSON = \case
    Public -> A.toJSON @Text "public"
    Private -> A.toJSON @Text "private"
instance A.FromJSON FieldVisibility where
  parseJSON = A.withText "visibility" \case
    "public" -> pure Public
    "private" -> pure Private
    other -> fail $ "expecting either 'public' or 'private', but found: '" <> T.unpack other <> "'"

data Field =
  Field
  { fDateModified :: UTCTime
  , fVisibility :: FieldVisibility
  , fValue :: T.Text
  }
  deriving stock (Show, Eq)
makeLensesWith abbreviatedFields ''Field

newField :: UTCTime -> T.Text -> Field
newField time value =
  Field
  { fDateModified = time
  , fVisibility = Public
  , fValue = value
  }

data Entry =
  Entry
  { ePath :: EntryPath
  , eDateModified :: UTCTime
  , eMasterField :: Maybe FieldKey
  , eFields :: HS.HashMap FieldKey Field
  , eTags :: S.Set EntryTag
  }
  deriving stock (Show, Eq)
makeLensesWith abbreviatedFields ''Entry

newEntry :: EntryPath -> UTCTime -> Entry
newEntry path time =
  Entry
  { ePath = path
  , eDateModified = time
  , eMasterField = Nothing
  , eFields = HS.empty
  , eTags = S.empty
  }

class EntryConvertible a where
  entry :: Prism a a Entry Entry
