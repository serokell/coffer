{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Entry
  ( newFieldKey
  , dateModified
  , Entry, EntryConvertible (..), newEntry
  , path, masterField, fields
  , Field (..), FieldKey,  newField, getFieldKey
  , private, value
  )
where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HS

import Control.Lens
import qualified Data.Aeson.Types as A
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.Time (UTCTime)

type DateTime = UTCTime

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

data Field =
  Field
  { _fDateModified :: DateTime
  , _private :: Bool
  , _value :: T.Text
  }
  deriving (Show, Eq)

newField :: UTCTime -> T.Text -> Field
newField time value =
  Field
  { _fDateModified = time
  , _private = False
  , _value = value
  }

makeLensesFor [("_value", "value"), ("_private", "private")] ''Field

data Entry =
  Entry
  { _path :: [T.Text]
  , _eDateModified :: DateTime
  , _masterField :: Maybe FieldKey
  , _fields :: HS.HashMap FieldKey Field
  }
  deriving (Show, Eq)

newEntry :: [T.Text] -> UTCTime -> Entry
newEntry path time =
  Entry
  { _path = path
  , _eDateModified = time
  , _masterField = Nothing
  , _fields = HS.empty
  }

makeLensesFor [("_path", "path"), ("_masterField", "masterField"), ("_fields", "fields")] ''Entry

class DateModified a where
  dateModified :: Lens' a DateTime

instance DateModified Entry where
  dateModified = lens _eDateModified (\e d -> e { _eDateModified = d } )

instance DateModified Field where
  dateModified = lens _fDateModified (\e d -> e { _fDateModified = d } )

class EntryConvertible a where
  entry :: Prism a a Entry Entry
