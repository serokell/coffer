-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Entry.Json where

import Coffer.Path qualified as Path
import Control.Lens hiding ((.=))
import Control.Monad (forM)
import Data.Aeson (FromJSON, ToJSON, Value, fromJSON, (.=))
import Data.Aeson qualified as A
import Data.Aeson.Lens qualified as A
import Data.Either.Extra (eitherToMaybe)
import Data.HashMap.Strict qualified as HS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Entry (Entry, EntryConvertible, Field, FieldValue(FieldValue))
import Entry qualified as E
import Fmt (pretty)

newtype JsonEntry = JsonEntry Value
  deriving stock (Show)
  deriving newtype (ToJSON, FromJSON)

fieldConverter :: (Prism Value Value Field Field)
fieldConverter = prism' to from
  where
    to :: Field -> Value
    to field =
      A.object
      [ "date_modified" .= (field ^. E.dateModified)
      , "visibility" .= (field ^. E.visibility)
      , "value" .= (field ^. E.value . E.fieldValue)
      ]
    from (A.Object o) = do
      dateModified <- HS.lookup "date_modified" o
        >>= \case A.String t -> Just t ; _ -> Nothing
        >>= iso8601ParseM . T.unpack
      value <- HS.lookup "value" o >>= \case
        A.String t -> Just t
        _ -> Nothing
      _visibility <- HS.lookup "visibility" o >>= resultToMaybe . fromJSON
      pure
        $ E.newField dateModified (FieldValue value)
        & E.visibility .~ _visibility
    from _ = Nothing

instance EntryConvertible JsonEntry where
  entry = prism' to from
    where
      to :: Entry -> JsonEntry
      to entry =
        JsonEntry $ A.object
        [ "path" .= pretty @_ @Text (entry ^. E.path)
        , "date_modified" .= (entry ^. E.dateModified)
        , "master_field" .= (entry ^. E.masterField)
        , "fields" .= (HS.fromList . over (each . _1) E.getFieldKey . HS.toList  . HS.map (^. re fieldConverter) $ (entry ^. E.fields))
        , "tags" .= (entry ^. E.tags)
        ]
      from (JsonEntry (A.Object o)) =
        do
          path <- HS.lookup "path" o
            >>= (\case A.String t -> Just t ; _ -> Nothing)
            >>= eitherToMaybe . Path.mkEntryPath
          dateModified <- HS.lookup "date_modified" o
            >>= \case A.String t -> Just t ; _ -> Nothing
            >>= iso8601ParseM . T.unpack
          let _masterField = HS.lookup "master_field" o >>= \case
                A.String t -> eitherToMaybe $ E.newFieldKey t
                _ -> Nothing
          _fields <- do
            value <- HS.lookup "fields" o
            obj <- value ^? A._Object
            keyFields <-
              forM (HS.toList obj) \(text, value) -> do
                key <- eitherToMaybe $ E.newFieldKey text
                field <- value ^? fieldConverter
                pure (key, field)
            pure $ HS.fromList keyFields
          _tags <- HS.lookup "tags" o >>= resultToMaybe . fromJSON

          pure
            $ E.newEntry path dateModified
            & E.masterField .~ _masterField
            & E.fields .~ _fields
            & E.tags .~ _tags
      from _ = Nothing

resultToMaybe :: A.Result a -> Maybe a
resultToMaybe = \case
  A.Error _ -> Nothing
  A.Success a -> Just a
