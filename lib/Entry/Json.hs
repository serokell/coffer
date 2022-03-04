-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Entry.Json where

import qualified Data.Aeson as A
import qualified Data.Aeson.Lens as A

import qualified Entry as E
import Control.Lens
import qualified Data.HashMap.Strict as HS
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Control.Monad (forM)
import Entry (Entry)
import Fmt (pretty)
import qualified Coffer.Path as Path
import Data.Either.Extra (eitherToMaybe)

newtype JsonEntry = JsonEntry A.Value
  deriving stock (Show)
  deriving newtype (A.ToJSON, A.FromJSON)

fieldConverter :: (Prism A.Value A.Value E.Field E.Field)
fieldConverter = prism' to from
  where to :: E.Field -> A.Value
        to field =
          A.object
          [ "date_modified" A..= (field ^. E.dateModified)
          , "visibility" A..= (field ^. E.visibility)
          , "value" A..= (field ^. E.value)
          ]
        from (A.Object o) = do
          dateModified <- HS.lookup "date_modified" o
                               >>= \case A.String t -> Just t ; _ -> Nothing
                               >>= iso8601ParseM . T.unpack
          value <- HS.lookup "value" o
                     >>= \case
                            A.String t -> Just t
                            _ -> Nothing
          _visibility <- HS.lookup "visibility" o >>= resultToMaybe . A.fromJSON
          pure
            $ E.newField dateModified value
            & E.visibility .~ _visibility
        from _ = Nothing

instance E.EntryConvertible JsonEntry where
  entry = prism' to from
    where to :: Entry -> JsonEntry
          to entry =
            JsonEntry $ A.object
            [ "path" A..= pretty @_ @T.Text (entry ^. E.path)
            , "date_modified" A..= (entry ^. E.dateModified)
            , "master_field" A..= (entry ^. E.masterField)
            , "fields" A..= (HS.fromList . over (each . _1) E.getFieldKey . HS.toList  . HS.map (^. re fieldConverter) $ (entry ^. E.fields))
            , "tags" A..= (entry ^. E.tags)
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
                      forM (HS.toList obj) $ \(text, value) -> do
                        key <- eitherToMaybe $ E.newFieldKey text
                        field <- value ^? fieldConverter
                        pure (key, field)
                   pure $ HS.fromList keyFields
                _tags <- HS.lookup "tags" o >>= resultToMaybe . A.fromJSON

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
