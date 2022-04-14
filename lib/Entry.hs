-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Entry
  ( FieldKey
  , keyCharSet
  , newFieldKey
  , getFieldKey
  , EntryTag
  , newEntryTag
  , getEntryTag
  , FieldVisibility(..)
  , FieldValue (..)
  , fieldValue
  , Field (..)
  , dateModified
  , newField
  , visibility
  , value
  , Entry
  , newEntry
  , path
  , masterField
  , fields
  , tags
  , EntryConvertible (..)
  )
where

import Coffer.Path (EntryPath)
import Control.Lens
import Data.Aeson qualified as A
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HS
import Data.Hashable (Hashable)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Fmt (Buildable, build)
import System.Console.ANSI (SGR(Reset), setSGRCode)
import System.Console.ANSI.Codes (csi)

newtype FieldKey = UnsafeFieldKey Text
  deriving stock (Show, Eq)
  deriving newtype (A.ToJSON, A.ToJSONKey, A.FromJSON, A.FromJSONKey, Hashable, Buildable)

keyCharSet :: [Char]
keyCharSet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_;"

newFieldKey :: Text -> Either Text FieldKey
newFieldKey t
  | T.null t =
      Left "Tags must contain at least 1 character"
  | T.any (`notElem` keyCharSet) t =
      Left $ "Tags can only contain the following characters: '" <> T.pack keyCharSet <> "'"
  | otherwise = Right $ UnsafeFieldKey t

getFieldKey :: FieldKey -> Text
getFieldKey (UnsafeFieldKey t) = t

newtype EntryTag = UnsafeEntryTag Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (A.ToJSON, A.FromJSON, Buildable)

newEntryTag :: Text -> Either Text EntryTag
newEntryTag tag
  | T.null tag =
      Left "Tags must contain at least 1 character"
  | T.any (`notElem` keyCharSet) tag =
      Left $ "Tags can only contain the following characters: '" <> T.pack keyCharSet <> "'"
  | otherwise = Right $ UnsafeEntryTag tag

getEntryTag :: EntryTag -> Text
getEntryTag (UnsafeEntryTag t) = t

data FieldVisibility = Public | Private
  deriving stock (Show, Eq)

instance Buildable FieldVisibility where
  build = \case
    Public -> "public"
    Private -> "private"

instance A.ToJSON FieldVisibility where
  toJSON = \case
    Public -> A.toJSON @Text "public"
    Private -> A.toJSON @Text "private"
instance A.FromJSON FieldVisibility where
  parseJSON = A.withText "visibility" \case
    "public" -> pure Public
    "private" -> pure Private
    other -> fail $ "expecting either 'public' or 'private', but found: '" <> T.unpack other <> "'"

newtype FieldValue = FieldValue { unFieldValue :: Text }
  deriving stock (Show, Eq, Ord)
makeLensesFor [("unFieldValue", "fieldValue")] ''FieldValue

-- | User can use ANSI control sequences in field values.
-- If some ANSI control sequence contain in field value then we append @reset@ ANSI control sequence.
-- Otherwise, we just return wrapped text.
-- You can see explanation here (https://github.com/serokell/coffer/issues/48)
instance Buildable FieldValue where
  build (FieldValue t) =
    if T.pack (csi [] "") `T.isInfixOf` t then
      build t <> build (setSGRCode [Reset])
    else
      build t

data Field =
  Field
  { fDateModified :: UTCTime
  , fVisibility :: FieldVisibility
  , fValue :: FieldValue
  }
  deriving stock (Show, Eq)
makeLensesWith abbreviatedFields ''Field

newField :: UTCTime -> FieldValue -> Field
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
  , eFields :: HashMap FieldKey Field
  , eTags :: Set EntryTag
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
