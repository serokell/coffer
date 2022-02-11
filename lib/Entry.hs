{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Entry
  ( dateModified
  , Entry, EntryConvertible (..), newEntry
  , path, masterField, fields
  , Field, FieldKey (..), newField, newFieldKey, getFieldKey, newEntryTag, getEntryTag
  , visibility, value, tags
  , FieldVisibility(..)
  )
where


import qualified Data.Text as T
import qualified Data.HashMap.Strict as HS

import Control.Lens
import qualified Data.Aeson as A
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Language.Haskell.TH.Lens (_CaseE)
import Data.Text (Text)
import Coffer.Path (EntryPath)

type DateTime = UTCTime

newtype FieldKey = FieldKey T.Text
  deriving (Generic, Show, Eq)

instance Hashable FieldKey

instance A.ToJSON FieldKey where
instance A.ToJSONKey FieldKey where
instance A.FromJSON FieldKey where
instance A.FromJSONKey FieldKey where

newFieldKey :: T.Text -> Maybe FieldKey
newFieldKey t =
  if T.foldr ((&&) . (`elem` allowedChars)) True t then
    Just $ FieldKey t
  else
    Nothing
  where allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_;:"

getFieldKey :: FieldKey -> T.Text
getFieldKey (FieldKey t) = t

newtype EntryTag = EntryTag T.Text
  deriving (Generic, Show, Eq)

instance A.ToJSON EntryTag where
instance A.FromJSON EntryTag where

newEntryTag :: T.Text -> Maybe EntryTag
newEntryTag t =
  if T.foldr ((&&) . (`elem` allowedChars)) True t then
    Just $ EntryTag t
  else
    Nothing
  where allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-"

getEntryTag :: EntryTag -> T.Text
getEntryTag (EntryTag t) = t

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
  { _fDateModified :: DateTime
  , _visibility :: FieldVisibility
  , _value :: T.Text
  }
  deriving (Show, Eq)

newField :: UTCTime -> T.Text -> Field
newField time value =
  Field
  { _fDateModified = time
  , _visibility = Private
  , _value = value
  }

makeLensesFor [("_value", "value"), ("_visibility", "visibility")] ''Field

data Entry =
  Entry
  { _path :: EntryPath
  , _eDateModified :: DateTime
  , _masterField :: Maybe FieldKey
  , _fields :: HS.HashMap FieldKey Field
  , _tags :: [EntryTag]
  }
  deriving (Show, Eq)

newEntry :: EntryPath -> UTCTime -> Entry
newEntry path time =
  Entry
  { _path = path
  , _eDateModified = time
  , _masterField = Nothing
  , _fields = HS.empty
  , _tags = []
  }

makeLensesFor [("_path", "path"), ("_masterField", "masterField"), ("_fields", "fields"), ("_tags", "tags")] ''Entry

class DateModified a where
  dateModified :: Lens' a DateTime

instance DateModified Entry where
  dateModified = lens _eDateModified (\e d -> e { _eDateModified = d } )

instance DateModified Field where
  dateModified = lens _fDateModified (\e d -> e { _fDateModified = d } )

class EntryConvertible a where
  entry :: Prism a a Entry Entry
