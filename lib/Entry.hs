{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Entry
  ( newFieldKey
  , dateModified
  , Entry (..), EntryConvertible (..), emptyEntry
  , path, masterField, fields
  , Field (..), FieldKey (..), emptyField, getFieldKey
  , value
  )
where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HS

import Control.Lens
import qualified Data.Aeson.Types as A
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- TODO - proper date time lib
type DateTime = T.Text

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

data Field =
  Field
  { _fDateModified :: DateTime
  , _value :: T.Text
  }
  deriving (Show, Eq)

-- TODO me no likey, better way? https://github.com/ekmett/lens/issues/286
emptyField :: Field
emptyField =
  Field
  { _fDateModified = ""
  , _value = ""
  }

makeLensesFor [("_value", "value")] ''Field

data Entry =
  Entry
  { _path :: [T.Text]
  , _eDateModified :: DateTime
  , _masterField :: (FieldKey, Field)
  , _fields :: HS.HashMap FieldKey Field
  }
  deriving (Show, Eq)

-- TODO me no likey, better way? https://github.com/ekmett/lens/issues/286
emptyEntry :: Entry
emptyEntry =
  Entry
  { _path = []
  , _eDateModified = ""
  , _masterField = undefined
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