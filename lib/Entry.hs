-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Entry
  ( FieldName
  , BadFieldName (..)
  , allowedCharSet
  , newFieldName
  , getFieldName
  , EntryTag
  , BadEntryTag (..)
  , newEntryTag
  , getEntryTag
  , FieldVisibility(..)
  , FieldContents (..)
  , fieldContents
  , Field (..)
  , dateModified
  , newField
  , visibility
  , contents
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
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HS
import Data.Hashable (Hashable)
import Data.OpenApi
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Fmt (Buildable, build)
import GHC.Generics (Generic)
import Servant (FromHttpApiData, ToHttpApiData)
import System.Console.ANSI (SGR(Reset), setSGRCode)
import System.Console.ANSI.Codes (csi)

newtype FieldName = UnsafeFieldName Text
  deriving stock (Show, Eq)
  deriving newtype (A.ToJSON, A.ToJSONKey, A.FromJSON, A.FromJSONKey, Hashable, Buildable, ToHttpApiData, FromHttpApiData)

instance ToSchema FieldName where
  declareNamedSchema proxy = pure $ NamedSchema (Just "FieldName") (toParamSchema proxy)

instance ToParamSchema FieldName where
  toParamSchema _ =
    mempty { _schemaPattern = Just fieldNamePattern }
      & type_ ?~ OpenApiString
    where
      fieldNamePattern = "[" <> T.pack allowedCharSet <> "]*"

allowedCharSet :: [Char]
allowedCharSet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_;"

newtype BadFieldName = BadFieldName { unBadFieldName :: Text }
  deriving newtype Buildable

newFieldName :: Text -> Either BadFieldName FieldName
newFieldName t
  | T.null t =
      Left $ BadFieldName "Tags must contain at least 1 character"
  | T.any (`notElem` allowedCharSet) t =
      Left $ BadFieldName ("Tags can only contain the following characters: '" <> T.pack allowedCharSet <> "'")
  | otherwise = Right $ UnsafeFieldName t

getFieldName :: FieldName -> Text
getFieldName (UnsafeFieldName t) = t

newtype EntryTag = UnsafeEntryTag Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (A.ToJSON, A.FromJSON, Buildable, Hashable, ToHttpApiData, FromHttpApiData)

instance ToSchema EntryTag where
  declareNamedSchema proxy = pure $ NamedSchema (Just "EntryTag") (toParamSchema proxy)

instance ToParamSchema EntryTag where
  toParamSchema _ =
    mempty { _schemaPattern = Just entryTagPattern }
      & type_ ?~ OpenApiString
    where
      entryTagPattern = "[" <> T.pack allowedCharSet <> "]*"

newtype BadEntryTag = BadEntryTag { unBadEntryTag :: Text }
  deriving newtype Buildable

newEntryTag :: Text -> Either BadEntryTag EntryTag
newEntryTag tag
  | T.null tag =
      Left $ BadEntryTag "Tags must contain at least 1 character"
  | T.any (`notElem` allowedCharSet) tag =
      Left $ BadEntryTag ("Tags can only contain the following characters: '" <> T.pack allowedCharSet <> "'")
  | otherwise = Right $ UnsafeEntryTag tag

getEntryTag :: EntryTag -> Text
getEntryTag (UnsafeEntryTag t) = t

data FieldVisibility = Public | Private
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)

instance ToSchema FieldVisibility where
  declareNamedSchema _ = pure $ NamedSchema (Just "FieldVisibility") $ mempty
    & type_ ?~ OpenApiString
    & enum_ ?~ ["public", "private"]

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

newtype FieldContents = FieldContents { unFieldContents :: Text }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable, A.FromJSON, A.ToJSON, A.FromJSONKey, A.ToJSONKey, ToSchema)
makeLensesFor [("unFieldContents", "fieldContents")] ''FieldContents

-- | User can use ANSI control sequences in field contents.
-- If some ANSI control sequence contain in field contents then we append @reset@ ANSI control sequence.
-- Otherwise, we just return wrapped text.
-- You can see explanation here (https://github.com/serokell/coffer/issues/48)
instance Buildable FieldContents where
  build (FieldContents t) =
    if T.pack (csi [] "") `T.isInfixOf` t then
      build t <> build (setSGRCode [Reset])
    else
      build t

data Field =
  Field
  { fDateModified :: UTCTime
  , fVisibility :: FieldVisibility
  , fContents :: FieldContents
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)
deriveToJSON (aesonPrefix camelCase) ''Field
makeLensesWith abbreviatedFields ''Field

instance ToSchema Field where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions (aesonPrefix camelCase)

newField :: UTCTime -> FieldContents -> Field
newField time contents =
  Field
  { fDateModified = time
  , fVisibility = Public
  , fContents = contents
  }

data Entry =
  Entry
  { ePath :: EntryPath
  , eDateModified :: UTCTime
  , eMasterField :: Maybe FieldName
  , eFields :: HashMap FieldName Field
  , eTags :: Set EntryTag
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable)
deriveToJSON (aesonPrefix camelCase) ''Entry
makeLensesWith abbreviatedFields ''Entry

instance ToSchema Entry where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions (aesonPrefix camelCase)

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
