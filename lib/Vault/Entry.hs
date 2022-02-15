{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Vault.Entry
  ( newFieldKey
  , dateModified
  , Entry (..), EntryConvertible (..), emptyEntry
  , path, masterField, fields
  , Field (..), FieldKey (..), emptyField
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
  toJSON (FieldKey text) = A.toJSON text

instance A.ToJSONKey FieldKey where

newFieldKey :: T.Text -> Maybe FieldKey
newFieldKey t =
  if T.foldr ((&&) . (`elem` allowedChars)) True t then
    Just $ FieldKey t
  else
    Nothing
  where allowedChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_;:"

data Field =
  Field
  { _fDateModified :: DateTime
  , _value :: T.Text
  }

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
  , _fields :: HS.HashMap FieldKey Field
  }

-- TODO me no likey, better way? https://github.com/ekmett/lens/issues/286
emptyEntry :: Entry
emptyEntry =
  Entry
  { _path = []
  , _eDateModified = ""
  , _fields = HS.empty
  }

makeLensesFor [("_path", "path"), ("_fields", "fields")] ''Entry

class DateModified a where
  dateModified :: Lens' a DateTime

instance DateModified Entry where
  dateModified = lens _eDateModified (\e d -> e { _eDateModified = d } )

instance DateModified Field where
  dateModified = lens _fDateModified (\e d -> e { _fDateModified = d } )

class EntryConvertible a where
  entry :: Prism a a Entry Entry
