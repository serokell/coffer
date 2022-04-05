-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

module Entry.Pass
  ( passTextPrism
  ) where

import Coffer.Path (entryPathAsPath, mkEntryPath)
import Control.Lens
import Control.Monad (guard)
import Data.Either.Extra (eitherToMaybe)
import Data.HashMap.Lazy qualified as HS
import Data.Maybe
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime(UTCTime, utctDay), secondsToDiffTime)
import Data.Time.Calendar.OrdinalDate.Compat
import Data.Time.Clock.Compat (UTCTime(utctDayTime))
import Data.Time.Format.ISO8601
import Data.Void
import Entry qualified as E
import Fmt
import Text.Megaparsec
import Text.Megaparsec.Char

data PassKv = PassKv (Maybe Text) [(Text, Text)]

data PassField =
  PassField
  { pfKey :: Text
  , pfDateModified :: Text
  , pfVisibility :: Text
  , pfValue :: Text
  }
  deriving stock (Show)
makeLensesWith abbreviatedFields ''PassField

data PassEntry =
  PassEntry
  { peMasterFieldKey :: Maybe Text
  , peMasterFieldValue :: Maybe Text
  , peDateModified :: Text
  , peFields :: [PassField]
  , peTags :: [Text]
  , pePath :: Text
  }
  deriving stock (Show)
makeLensesWith abbreviatedFields ''PassEntry

passFieldPrism :: Prism' PassField (E.FieldKey, E.Field)
passFieldPrism = prism' to from
  where to :: (E.FieldKey, E.Field) -> PassField
        to (fieldKey, field) = PassField
                { pfKey = E.getFieldKey fieldKey
                , pfDateModified =
                  T.pack . iso8601Show $
                  field ^. E.dateModified
                , pfVisibility = case field ^. E.visibility of
                                   E.Public -> "public"
                                   E.Private -> "private"
                , pfValue = E.unFieldValue $ field ^. E.value
                }
        from :: PassField -> Maybe (E.FieldKey, E.Field)
        from passField = do
          let fieldValue = E.FieldValue $ passField ^. value
          fieldKey <- passField ^. key & E.newFieldKey & eitherToMaybe
          dateModified <- iso8601ParseM . T.unpack $  passField ^. dateModified
          visibility <- case passField ^. visibility of
                          "public" -> Just E.Public
                          "private" -> Just E.Private
                          _ -> Nothing

          pure ( fieldKey
               , E.newField dateModified fieldValue
                 & E.visibility .~ visibility
               )

instance E.EntryConvertible PassEntry where
  entry = prism' to from
    where to :: E.Entry -> PassEntry
          to entry =
            PassEntry
            { peMasterFieldKey = entry ^. E.masterField <&> E.getFieldKey
            , peMasterFieldValue =
                (entry ^. E.masterField)
                >>= \fk -> entry ^. E.fields . at fk
                        <&> (E.unFieldValue . (^. E.value))
            , peDateModified =
                  T.pack . iso8601Show $
                  entry ^. E.dateModified
            , peFields =
              map (^. re passFieldPrism) (entry ^. E.fields & HS.toList)
            , peTags = entry ^. E.tags & S.toList & map E.getEntryTag
            , pePath = entry ^. E.path & entryPathAsPath & build & fmt
            }
          from :: PassEntry -> Maybe E.Entry
          from passEntry = do
            let masterField = passEntry ^. masterFieldKey
            dateModified <- passEntry ^. dateModified & iso8601ParseM . T.unpack
            fields <- mapM (^? passFieldPrism) (passEntry ^. fields)
                      <&> HS.fromList
            tags <- mapM (eitherToMaybe . E.newEntryTag) (passEntry ^. tags)
                    <&> S.fromList
            entryPath <- eitherToMaybe . mkEntryPath $ passEntry ^. path

            pure $ E.newEntry entryPath dateModified
              & E.fields .~ fields
              & E.tags .~ tags
              & E.masterField .~ (masterField >>= eitherToMaybe . E.newFieldKey)

instance E.EntryConvertible PassKv where
  entry = prism' to from
    where to :: E.Entry -> PassKv
          to entry =
            let passEntry = entry ^. re E.entry :: PassEntry
                masterValue = passEntry ^. masterFieldValue
            in PassKv masterValue . onlyJust $
               concat (flip map (passEntry ^. fields)
                 \field ->
                   map Just
                   [ (field ^. key, field ^. value)
                   , ("#$" <> field ^. key <> ".DATE_MODIFIED", field ^. dateModified)
                   , ("#$" <> field ^. key <> ".VISIBILITY", field ^. visibility)
                   ])
               <>
               [ Just ("#$DATE_MODIFIED", passEntry ^. dateModified)
               , Just ("#$TAGS", T.intercalate "," (passEntry ^. tags))
               , Just ("#$PATH", passEntry ^. path)
               ]

          onlyJust :: [Maybe a] -> [a]
          onlyJust = unsafeOnlyJust . filter isJust
          unsafeOnlyJust :: [Maybe a] -> [a]
          unsafeOnlyJust = map $
            \case Just a -> a
                  Nothing -> undefined
          isJust :: Maybe a -> Bool
          isJust = \case Just _ -> True
                         Nothing -> False

          from :: PassKv -> Maybe E.Entry
          from (PassKv masterValue passKv) = do
            let hs = HS.fromList passKv
                fhs = HS.fromList $ map (\(a, b) -> (b, a)) passKv
                utcUnixEpoch = iso8601Show $
                               UTCTime
                               { utctDay = YearDay 0 0
                               , utctDayTime = secondsToDiffTime 0
                               }
                masterKey = masterValue >>= \a -> fhs ^. at a
                dateModified = fromMaybe
                               (T.pack utcUnixEpoch)
                               (hs ^. at "#$DATE_MODIFIED")
                tags = fromMaybe
                       []
                       (hs ^. at "#$TAGS"
                        <&> T.split (== ',')
                        <&> filter (/= ""))
                fields = hs
                       & HS.filterWithKey (\k _v -> T.take 2 k /= "#$")
                       & HS.mapKeys (T.split (== '.'))
                       & HS.filterWithKey (\k _v -> length k == 1)
                       & HS.mapKeys head
                       & HS.toList
                       & mapM \(k, v) ->
                               PassField k
                                 <$> hs ^. at ("#$" <> k <> ".DATE_MODIFIED")
                                 <*> hs ^. at ("#$" <> k <> ".VISIBILITY")
                                 <*> pure v
                entryPath = hs ^. at "#$PATH"



            guard (isNothing masterValue == isNothing masterKey)

            PassEntry
              masterKey
              masterValue
              dateModified
              <$> fields
              <*> pure tags
              <*> entryPath
              >>=  (^? E.entry)

type Parser = Parsec Void Text

passTextPrism :: Prism' Text PassKv
passTextPrism = prism' to from
  where to :: PassKv -> Text
        to (PassKv masterValue hs) =
          fromMaybe "" masterValue
          <> "\n\n"
          <>
          (map (\(k, v) -> k <> "=" <> v) hs
          & T.intercalate "\n")

        from :: Text -> Maybe PassKv
        from text = parseMaybe parser text
          where parseLine
                  :: Maybe String
                  -> Parser Text
                parseLine label =
                  do
                    x <- takeWhileP label (/='\n')
                    try newline
                    pure x

                parsePair
                  :: Parser (Text, Text)
                parsePair = do
                   key <- takeWhileP (Just "key") (/='=')
                   char '='
                   value <- takeWhileP (Just "value") (/='\n')
                   char '\n' <|> pure '\n'
                   pure (key, value)

                parser :: Parser PassKv
                parser = do
                  masterValue <- parseLine (Just "character")
                                 <&> \case "" -> Nothing
                                           a -> Just a
                  takeWhileP (Just "empty line") (=='\n')

                  pairs <- many parsePair
                  eof
                  pure (PassKv masterValue pairs)
